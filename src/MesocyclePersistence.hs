{-# LANGUAGE DeriveGeneric #-}
module MesocyclePersistence (
        saveMesocycle
    , loadMesocycle
    , dataDir
    , mesocycleFile
    , loadOrInitMesocycle
) where

import Mesocycle
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Exception (catch, IOException)

-- Derive ToJSON/FromJSON for all relevant types


-- Save a Mesocycle to a JSON file
saveMesocycle :: FilePath -> Mesocycle -> IO ()
saveMesocycle path meso = BL.writeFile path (encode meso)

-- Load a Mesocycle from a JSON file
loadMesocycle :: FilePath -> IO (Maybe Mesocycle)
loadMesocycle path = decode <$> BL.readFile path

-- Directory and default file path for mesocycle persistence
dataDir :: FilePath
dataDir = "data"

mesocycleFile :: FilePath
mesocycleFile = dataDir ++ "/mesocycle.json"

-- | Load existing mesocycle from default file, initializing with provided plan if absent or unreadable.
loadOrInitMesocycle :: Mesocycle -> IO Mesocycle
loadOrInitMesocycle initial = do
    createDirectoryIfMissing True dataDir
    exists <- doesFileExist mesocycleFile
    if exists
        then do
            m <- (loadMesocycle mesocycleFile) `catch` handleLoad
            case m of
                Just p -> pure p
                Nothing -> saveAndReturn
        else saveAndReturn
    where
        saveAndReturn = saveMesocycle mesocycleFile initial >> pure initial
        handleLoad :: IOException -> IO (Maybe Mesocycle)
        handleLoad _ = pure Nothing
