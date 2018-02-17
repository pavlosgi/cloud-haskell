{-# LANGUAGE OverloadedStrings #-}

module CommandLineParser
  ( parseArgs
  ) where

import Types

import           Control.Lens         ((^?), element)
import qualified Data.Validation      as V
import qualified Data.List.NonEmpty   as L.NE
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.NonEmpty        as NE
import           Data.NonEmpty        ((!:))
import qualified Data.List            as L
import qualified Data.Maybe           as Mb
import qualified Data.Set             as Set
import qualified Data.Text            as Tx
import qualified System.Directory     as S.D
import qualified System.Environment   as S.E
import qualified System.IO            as S.IO
import qualified Text.Read            as Tx.R

parseArgs :: IO (V.AccValidation (L.NE.NonEmpty ConfigError) Config)
parseArgs
  = do
      args <- S.E.getArgs
      case args of
        [] -> pure $ V.AccFailure (EmptyArgumentList :| [])
        args' -> parseArgs' args'

parseArgs'
  :: [String]
  -> IO (V.AccValidation (L.NE.NonEmpty ConfigError) Config)

parseArgs' args
  = buildConfig $ (,,,) <$>
      parseMaybe "--node-file" (pure . File) args <*>
      parse "--send-for" mkSeconds args <*>
      parse "--wait-for" mkSeconds args <*>
      parse "--with-seed" mkSeed args

  where
    mkSeconds str
      = case Tx.R.readMaybe (Tx.unpack str) of
          Nothing -> V.AccFailure (InvalidSeconds str :| [])
          Just s | s >= 0 -> V.AccSuccess (Seconds s)
          Just _ -> V.AccFailure (InvalidSeconds str :| [])

    mkSeed str
      = case Tx.R.readMaybe (Tx.unpack str) of
          Nothing -> V.AccFailure (InvalidSeconds str :| [])
          Just s -> V.AccSuccess (Seed s)


    parseMaybe name build args'
      = fromEither $ do
          case L.elemIndex (Tx.unpack name) args' of
            Nothing -> pure Nothing
            Just argIndex -> do
              argValue <- args' ^? element (argIndex + 1) `failIfNothing`
                NoValueForArgument name :| []

              fromValidation (Just <$> build (Tx.pack argValue))

    parse name build args'
      = case parseMaybe name build args' of
           V.AccFailure err -> V.AccFailure err
           V.AccSuccess Nothing -> V.AccFailure (ArgumentWithNameDoesNotExist name :| [])
           V.AccSuccess (Just v) -> pure v

    fromEither ei
      = case ei of
          Left err -> V.AccFailure err
          Right v -> V.AccSuccess v

    fromValidation v
      = case v of
          V.AccFailure err -> Left err
          V.AccSuccess v' -> Right v'

    failIfNothing mb err
      = case mb of
          Nothing -> Left err
          Just v -> Right v

    infixl 1 `failIfNothing`

    parseEndpoints f
      = do
          content <- S.IO.readFile (Tx.unpack f)
          case L.lines content of
            [] -> pure (V.AccFailure (EmptyNodesConfig (File f) :| []))
            n:ns ->
              let neL = Endpoint . Tx.pack <$> n !: ns
              in pure (V.AccSuccess (NE.mapTail Set.fromList neL))

    buildConfig vf
      = case vf of
          V.AccFailure err -> pure (V.AccFailure err)
          V.AccSuccess (file, sf, wf, ws) ->
            let file' = Mb.fromMaybe (File "nodes.conf") file
                file'' = _fileText file'

            in do
                 exists <- S.D.doesFileExist (Tx.unpack file'')
                 endpoints <- if exists
                   then parseEndpoints file''
                   else pure (V.AccFailure (FileDoesNotExist file' :| []))

                 case endpoints of
                   V.AccFailure err -> pure (V.AccFailure err)
                   V.AccSuccess e -> pure (V.AccSuccess (Config e sf wf ws))

