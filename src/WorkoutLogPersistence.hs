{-# LANGUAGE DeriveGeneric #-}
module WorkoutLogPersistence where

import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import WorkoutLog
import System.Directory (doesFileExist)


-- | Save a WorkoutLog to a JSON file
saveWorkoutLog :: FilePath -> WorkoutLog -> IO ()
saveWorkoutLog path logData = BL.writeFile path (encode logData)

-- | Load a WorkoutLog from a JSON file
loadWorkoutLog :: FilePath -> IO (Maybe WorkoutLog)
loadWorkoutLog path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else do
      content <- BL.readFile path
      return (decode content)
