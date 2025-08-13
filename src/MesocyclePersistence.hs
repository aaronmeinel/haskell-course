{-# LANGUAGE DeriveGeneric #-}
module MesocyclePersistence (
    saveMesocycle, loadMesocycle
) where

import Mesocycle
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)

-- Derive ToJSON/FromJSON for all relevant types
instance ToJSON Mesocycle
instance FromJSON Mesocycle
instance ToJSON MesocycleWeek
instance FromJSON MesocycleWeek
instance ToJSON MesocycleWorkout
instance FromJSON MesocycleWorkout
instance ToJSON MesocycleExercise
instance FromJSON MesocycleExercise
instance ToJSON PreExerciseFeedback
instance FromJSON PreExerciseFeedback
instance ToJSON PostExerciseFeedback
instance FromJSON PostExerciseFeedback
instance ToJSON Soreness
instance FromJSON Soreness
instance ToJSON JointPain
instance FromJSON JointPain
instance ToJSON Pump
instance FromJSON Pump
instance ToJSON Workload
instance FromJSON Workload

-- Save a Mesocycle to a JSON file
saveMesocycle :: FilePath -> Mesocycle -> IO ()
saveMesocycle path meso = BL.writeFile path (encode meso)

-- Load a Mesocycle from a JSON file
loadMesocycle :: FilePath -> IO (Maybe Mesocycle)
loadMesocycle path = decode <$> BL.readFile path
