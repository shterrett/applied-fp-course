{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.Conf
    ( parseOptions
    ) where

import           GHC.Word                  (Word16)

import           Data.Bifunctor            (first)
import           Data.Monoid               ((<>)
                                           , mconcat
                                           , Last(Last, getLast))

import           FirstApp.Types            ( Conf(Conf)
                                           , ConfigError(NoPort, NoDb)
                                           , DBFilePath (DBFilePath)
                                           , PartialConf(PartialConf)
                                           , Port (Port))

import           FirstApp.Conf.CommandLine (commandLineParser)
import           FirstApp.Conf.File        (parseJSONConfigFile)

-- For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf = PartialConf (mkLast $ Port 8080) (mkLast $ DBFilePath "app_db.db")
  where mkLast = Last . Just

-- We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig (PartialConf p db) =
    failWith NoPort p Conf >>=
      failWith NoDb db
    where failWith e m f = case getLast (f <$> m) of
                           Just r -> Right r
                           Nothing -> Left e

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.

-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions path = do
    fmCmd <- commandLineParser
    fmFile <- parseJSONConfigFile path
    return $ (fmap $ mconcat) (sequence [Right defaultConf, fmFile, Right fmCmd]) >>= makeConfig
  -- Parse the options from the config file: "appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
