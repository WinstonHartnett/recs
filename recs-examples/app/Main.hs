module Main where

import Recs.Lib
import qualified Data.Vector.Unboxed as VU
import Control.Monad.State.Strict
import Data.Maybe (fromJust)
import Control.Monad
import Data.Default
import Linear.V3
import Linear.V4
import Linear.Matrix
import           Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import Data.Word

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

myFunc :: Ecs ()
myFunc = do
  -- createArch []
  -- ecs <- get
  -- let spawnSys :: Commands -> System ()
  --     spawnSys c = do
  --       VU.forM_ [0..100_000] (\(_ :: Word32) -> void $ spawn c 
  --         >>= tagged (MkRotation emptyV3) 
  --         >>= tagged (MkVelocity emptyV3)
  --         >>= tagged (MkPosition emptyV3) 
  --         >>= tagged (MkTransform emptyM44)
  --         >>= finish)
  --     posSys :: Query (Nab Position) -> System ()
  --     posSys q = forQ q $ \qh -> do
  --       p <- nab @Position qh
  --       liftIO . print $ p
  -- (_, (MkSystemState commands)) <- queried spawnSys >>= runSystem
  -- commitCommands (fromJust commands) >> queried posSys >>= runSystem
--   let posSys (q :: Query (Nab Position |&| Nab Velocity)) = forQ q $ \qh -> do
--         p <- nab @Position qh
--         v <- nab @Velocity qh
--         liftIO . putStrLn $ show p <> " " <> show v
--       mySys :: Query (Nab Position |&| Nab Velocity) -> System ()
--       mySys q = forQ q $ \qh -> do
--         p <- nab @Position qh
--         liftIO . print $ p
--       myMaker c =
--         VU.forM_ [0..1000] (\p -> void $ spawn c >>= tagged (MkPosition p) >>= tagged (MkVelocity p) >>= finish)
--   (_, (MkSystemState commands)) <- queried myMaker >>= runSystem
  -- commitCommands (fromJust commands) >> queried posSys >>= runSystem
  pure ()

main :: IO ()
main = void $ runStateT (unEcs myFunc) =<< def