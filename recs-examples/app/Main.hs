{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Recs.Lib
import qualified Data.Vector.Unboxed as VU
-- import Control.Monad.State.Strict
import Data.Maybe (fromJust)
import Control.Monad
import Data.Default
import Linear.V3
import Linear.V4
import Linear.Matrix
import Data.Vector.Unboxed.Deriving ( derivingUnbox )
import GHC.Generics (Generic)
import Data.Word
import Effectful
import Effectful.State.Static.Local
import Data.Coerce (coerce)
import Criterion.Main
import Control.DeepSeq (NFData)

newtype Velocity = MkVelocity (V3 Float)

instance Identify Velocity

derivingUnbox "Velocity" [t|Velocity -> V3 Float|] [|\(MkVelocity v) -> v|] [|MkVelocity|]

newtype Position = MkPosition (V3 Float)
  deriving Show

instance Identify Position

derivingUnbox "Position" [t|Position -> V3 Float|] [|\(MkPosition v) -> v|] [|MkPosition|]

newtype Rotation = MkRotation (V3 Float)

instance Identify Rotation

derivingUnbox "Rotation" [t|Rotation -> V3 Float|] [|\(MkRotation v) -> v|] [|MkRotation|]

newtype Transform = MkTransform (M44 Float)

instance Identify Transform

derivingUnbox "Transform" [t|Transform -> M44 Float|] [|\(MkTransform v) -> v|] [|MkTransform|]

emptyV3 :: V3 Float
emptyV3 = V3 1.0 0.0 0.0

emptyM44 :: M44 Float
emptyM44 = (V4 (V4 1.0 0.0 0.0 0.0) (V4 1.0 0.0 0.0 0.0) (V4 1.0 0.0 0.0 0.0) (V4 1.0 0.0 0.0 0.0))

mySpawnF :: System EntityId
mySpawnF = spawn undefined do
  tagged $ MkRotation emptyV3
  tagged $ MkVelocity emptyV3
  tagged $ MkPosition emptyV3
  tagged $ MkTransform emptyM44

-- Goal
--
-- spawn c do
--   tagged $ MkRotation emptyV3
--   tagged $ MkVelocity emptyV3
--   tagged $ MkPosition emptyV3
--   tagged $ MkTransform emptyM44

newtype A = MkA {unA :: Int }
  deriving (Generic, NFData)
newtype B = MkB { unB :: Int}
  deriving (Generic, NFData)
newtype C = MkC Int
  deriving (Generic, NFData)
newtype D = MkD Int
  deriving (Generic, NFData)
newtype E = MkE Int
  deriving (Generic, NFData)

data Combined = MkCombined
      { af :: !A
      , bf :: !B
      , cf :: !C
      , df :: !D
      , ef :: !E
      }
    deriving (Generic, NFData)

modifyAll :: State Combined :> es => Eff es ()
modifyAll = do
  modify \c ->
    MkCombined
      { af = MkA $ (+1) (unA $ af c)
      , bf = MkB $ (+1) (unB $ bf c)
      , cf = coerce @(Int -> Int) (+1) (cf c)
      , df = coerce @(Int -> Int) (+1) (df c)
      , ef = coerce @(Int -> Int) (+1) (ef c)
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

main :: IO ()
main = defaultMain [
  bench "modifyAll" $ nf (runPureEff . runState (MkCombined (MkA 0) (MkB 1) (MkC 2) (MkD 3) (MkE 4))) (replicateM_ 1000 modifyAll),
  bench "modifyInd" $ nf (runPureEff . runState (MkA 0) . runState (MkB 0) . runState (MkC 0) . runState (MkD 0) . runState (MkE 0)) (replicateM_ 1000 modInd)
  ]






























