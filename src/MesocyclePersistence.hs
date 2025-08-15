{-# LANGUAGE DeriveGeneric #-}
module MesocyclePersistence (
    saveMesocycle, loadMesocycle
) where

import Mesocycle
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL

-- Derive ToJSON/FromJSON for all relevant types


-- Save a Mesocycle to a JSON file
saveMesocycle :: FilePath -> Mesocycle -> IO ()
saveMesocycle path meso = BL.writeFile path (encode meso)

-- Load a Mesocycle from a JSON file
loadMesocycle :: FilePath -> IO (Maybe Mesocycle)
loadMesocycle path = decode <$> BL.readFile path
