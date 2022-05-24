{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

-- import Recs.Lib

import Data.Vector.Unboxed qualified as VU
import Recs

-- import Control.Monad.State.Strict

import Control.DeepSeq (NFData, force)
import Control.Monad
import Control.Monad.Identity qualified as ST
import Control.Monad.State.Strict qualified as ST
import Criterion.Main
import Data.Coerce (coerce)
import Data.Default
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Growable qualified as VR
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word
import Effectful
import Effectful.Prim (Prim, runPrim)
import Effectful.State.Static.Local
import GHC.Base (Array#, Int (I#), Int#, closureSize#, unpackClosure#)
import GHC.DataSize
import GHC.Generics (Generic)
import GHC.Prim (sizeofArray#, sizeofByteArray#)
import Linear.Matrix
import Linear.V3
import Linear.V4
import qualified Data.Sequence as SQ

newtype Velocity = MkVelocity (V3 Float)

derivingUnbox "Velocity" [t|Velocity -> V3 Float|] [|\(MkVelocity v) -> v|] [|MkVelocity|]

instance Component Velocity where
  type Layout Velocity = VR.GrowableUnboxedIOVector Velocity

newtype Position = MkPosition (V3 Float)
  deriving (Show)

-- instance Identify Position

derivingUnbox "Position" [t|Position -> V3 Float|] [|\(MkPosition v) -> v|] [|MkPosition|]

instance Component Position where type Layout Position = VR.GrowableUnboxedIOVector Position

newtype Rotation = MkRotation (V3 Float)

-- instance Identify Rotation

derivingUnbox "Rotation" [t|Rotation -> V3 Float|] [|\(MkRotation v) -> v|] [|MkRotation|]

instance Component Rotation where type Layout Rotation = VR.GrowableUnboxedIOVector Rotation

newtype Transform = MkTransform (M44 Float)

-- instance Identify Transform

derivingUnbox "Transform" [t|Transform -> M44 Float|] [|\(MkTransform v) -> v|] [|MkTransform|]

instance Component Transform where type Layout Transform = VR.GrowableUnboxedIOVector Transform

emptyV3 :: V3 Float
emptyV3 = V3 1.0 0.0 0.0

emptyM44 :: M44 Float
emptyM44 = (V4 (V4 1.0 0.0 0.0 0.0) (V4 1.0 0.0 0.0 0.0) (V4 1.0 0.0 0.0 0.0) (V4 1.0 0.0 0.0 0.0))

-- mySpawnF :: System EntityId
-- mySpawnF = spawn undefined do
--  tagged $ MkRotation emptyV3
--  tagged $ MkVelocity emptyV3
--  tagged $ MkPosition emptyV3
--  tagged $ MkTransform emptyM44

---- Goal
----
---- spawn c do
----   tagged $ MkRotation emptyV3
----   tagged $ MkVelocity emptyV3
----   tagged $ MkPosition emptyV3
----   tagged $ MkTransform emptyM44

newtype A = MkA {unA :: Int}
  deriving (Generic, NFData)
newtype B = MkB {unB :: Int}
  deriving (Generic, NFData)
newtype C = MkC Int
  deriving (Generic, NFData)
newtype D = MkD Int
  deriving (Generic, NFData)
newtype E = MkE Int
  deriving (Generic, NFData)

data Combined = MkCombined
  { af :: {-# UNPACK #-} !A
  , bf :: {-# UNPACK #-} !B
  , cf :: {-# UNPACK #-} !C
  , df :: {-# UNPACK #-} !D
  , ef :: {-# UNPACK #-} !E
  }
  deriving (Generic)

instance NFData Combined

modifyAll :: State Combined :> es => Eff es ()
modifyAll = do
  modify \c ->
    MkCombined
      { af = force $ MkA $ (+ 1) (unA $ af c)
      , bf = force $ MkB $ (+ 1) (unB $ bf c)
      , cf = force $ coerce @(Int -> Int) (+ 1) (cf c)
      , df = force $ coerce @(Int -> Int) (+ 1) (df c)
      , ef = force $ coerce @(Int -> Int) (+ 1) (ef c)
      }

modA :: State A :> es => Eff es ()
modA = modify \(MkA a) -> MkA (a + 1)

modB :: State B :> es => Eff es ()
modB = modify \(MkB a) -> MkB (a + 1)

modC :: State C :> es => Eff es ()
modC = modify \(MkC a) -> MkC (a + 1)

modD :: State D :> es => Eff es ()
modD = modify \(MkD a) -> MkD (a + 1)

modE :: State E :> es => Eff es ()
modE = modify \(MkE a) -> MkE (a + 1)

modInd :: (State A :> es, State B :> es, State C :> es, State D :> es, State E :> es) => Eff es ()
modInd = modA >> modB >> modC >> modD >> modE

newtype CombinedT m a = MkCombinedT {unCombined :: ST.StateT Combined m a}
  deriving (Applicative, Functor, Monad, ST.MonadState Combined)

modifyST :: Monad m => CombinedT m ()
modifyST = do
  ST.modify \c ->
    MkCombined
      { af = force $ MkA $ (+ 1) (unA $ af c)
      , bf = force $ MkB $ (+ 1) (unB $ bf c)
      , cf = force $ coerce @(Int -> Int) (+ 1) (cf c)
      , df = force $ coerce @(Int -> Int) (+ 1) (df c)
      , ef = force $ coerce @(Int -> Int) (+ 1) (ef c)
      }

modifyF :: Combined -> Combined
modifyF = go 100000
 where
  go 0 !com = com
  go !idx !com =
    go
      (idx - 1)
      MkCombined
        { af = force $ coerce @(Int -> Int) (+ 1) (af com)
        , bf = force $ coerce @(Int -> Int) (+ 1) (bf com)
        , cf = force $ coerce @(Int -> Int) (+ 1) (cf com)
        , df = force $ coerce @(Int -> Int) (+ 1) (df com)
        , ef = force $ coerce @(Int -> Int) (+ 1) (ef com)
        }

-- go !idx !com =
-- myFunc :: Ecs ()
-- myFunc = do
--   -- createArch []
--   -- ecs <- get
--   -- let spawnSys :: Commands -> System ()
--   --     spawnSys c = do
--   --       VU.forM_ [0..100_000] (\(_ :: Word32) -> void $ spawn c
--   --         >>= tagged (MkRotation emptyV3)
--   --         >>= tagged (MkVelocity emptyV3)
--   --         >>= tagged (MkPosition emptyV3)
--   --         >>= tagged (MkTransform emptyM44)
--   --         >>= finish)
--   --     posSys :: Query (Nab Position) -> System ()
--   --     posSys q = forQ q $ \qh -> do
--   --       p <- nab @Position qh
--   --       liftIO . print $ p
--   -- (_, (MkSystemState commands)) <- queried spawnSys >>= runSystem
--   -- commitCommands (fromJust commands) >> queried posSys >>= runSystem
-- --   let posSys (q :: Query (Nab Position |&| Nab Velocity)) = forQ q $ \qh -> do
-- --         p <- nab @Position qh
-- --         v <- nab @Velocity qh
-- --         liftIO . putStrLn $ show p <> " " <> show v
-- --       mySys :: Query (Nab Position |&| Nab Velocity) -> System ()
-- --       mySys q = forQ q $ \qh -> do
-- --         p <- nab @Position qh
-- --         liftIO . print $ p
-- --       myMaker c =
-- --         VU.forM_ [0..1000] (\p -> void $ spawn c >>= tagged (MkPosition p) >>= tagged (MkVelocity p) >>= finish)
-- --   (_, (MkSystemState commands)) <- queried myMaker >>= runSystem
--   -- commitCommands (fromJust commands) >> queried posSys >>= runSystem
--   pure ()

-- testEff ::
--   Eff
--     '[ CommandBuilderE
--         '[ State (SystemState '[State World, Prim, IOE])
--          , State World
--          , Prim
--          , IOE
--          ]
--      , State (SystemState '[State World, Prim, IOE])
--      , State World
--      , Prim
--      , IOE
--      ]
--     ()
-- testEff = tagged $ MkVelocity $ V3 1.0 0.0 0.0

testWorld :: IO ()
testWorld = do
  wld <- def @(IO World)
  (_, wld') <- runEff . runPrim . runState wld $ do
    (_, ss) <- runSystem do
      spawn do
        tagged $ MkPosition $ V3 0.0 0.0 0.0
        tagged $ MkVelocity $ V3 1.0 0.0 0.0
    flushEntities \eId er -> allocateEntityIntoEmpty eId
    commitCommands ss.commands
  putStrLn "test"
  -- let c = ss.commands.unCommands
  -- putStrLn $ show $ length $ queue . fromJust $ SQ.lookup 0 c

main :: IO ()
main = testWorld

-- main = putStrLn . show $ I# (closureSize# (1 :: Int, 1 :: Int))
-- case unpackClosure# (1 :: Int, 1 :: Int) of
--   (# iptr, ba, arr #) -> do
--     putStrLn $ show 8
--     putStrLn $ show $ I# (sizeofByteArray# ba)
--     putStrLn $ show $ (I# (sizeofArray# arr)) * 8
--     pure ()
-- main =
--   defaultMain
--     [ bench "modifyAll" $ nf (runPureEff . runState (MkCombined (MkA 0) (MkB 1) (MkC 2) (MkD 3) (MkE 4))) (replicateM_ 10000 modifyAll)
--     , bench "modifyInd" $ nf (runPureEff . runState (MkA 0) . runState (MkB 0) . runState (MkC 0) . runState (MkD 0) . runState (MkE 0)) (replicateM_ 10000 modInd)
--     , bench "modifyST" $ nf (ST.runIdentity . flip ST.runStateT (MkCombined (MkA 0) (MkB 1) (MkC 2) (MkD 3) (MkE 4))) (replicateM_ 10000 $ unCombined modifyST)
--     , bench "pure" $ nf (\f -> f $ MkCombined (MkA 0) (MkB 1) (MkC 2) (MkD 3) (MkE 4)) modifyF
--     ]
