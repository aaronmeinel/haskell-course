{-# LANGUAGE DeriveGeneric #-}
module WorkoutLogPersistence where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL
import WorkoutLog

instance ToJSON SetLog
instance FromJSON SetLog
instance ToJSON FeedbackLog
instance FromJSON FeedbackLog
instance ToJSON WorkoutLog
instance FromJSON WorkoutLog

-- | Save a WorkoutLog to a JSON file
saveWorkoutLog :: FilePath -> WorkoutLog -> IO ()
saveWorkoutLog path logData = BL.writeFile path (encode logData)

-- | Load a WorkoutLog from a JSON file
loadWorkoutLog :: FilePath -> IO (Maybe WorkoutLog)
loadWorkoutLog path = do
  content <- BL.readFile path
  return (decode content)
