{-# LANGUAGE OverloadedStrings #-}

module CliParser
  ( parseArgs
  ) where

import Types

import           Control.Lens            ((^?), element)
import qualified Data.Validation         as V
import qualified Data.List.NonEmpty      as L.NE
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.NonEmpty           as NE
import           Data.NonEmpty           ((!:))
import qualified Data.List               as L
import qualified Data.Maybe              as Mb
import qualified Data.Set                as Set
import qualified Data.Semigroup          as Semi
import qualified Data.Text               as Tx
import qualified System.Directory        as S.D
import qualified System.Environment      as S.E
import qualified System.IO               as S.IO
import qualified Text.Read               as Tx.R

parseArgs :: IO (V.AccValidation (L.NE.NonEmpty ConfigError) Config)
parseArgs
  = do
      args <- S.E.getArgs
      case args of
        [] -> pure $ V.AccFailure (EmptyArgumentList :| [])
        args' -> parseArgs' args'

  where
    parseArgs' args
      = buildConfig $ (,,,) <$>
          parseMaybe "--node-file" (pure . File) args <*>
          parse "--send-for" mkSeconds args <*>
          parse "--wait-for" mkSeconds args <*>
          parse "--with-seed" mkSeed args

mkSeconds :: Tx.Text -> V.AccValidation (L.NE.NonEmpty ConfigError) Seconds
mkSeconds str
  = case Tx.R.readMaybe (Tx.unpack str) of
      Nothing -> V.AccFailure (InvalidSeconds str :| [])
      Just s | s > 0 -> V.AccSuccess (Seconds s)
      Just _ -> V.AccFailure (InvalidSeconds str :| [])

mkSeed :: Tx.Text -> V.AccValidation (L.NE.NonEmpty ConfigError) Seed
mkSeed str
  = case Tx.R.readMaybe (Tx.unpack str) of
      Nothing -> V.AccFailure (InvalidSeconds str :| [])
      Just s | s > 0 -> V.AccSuccess (Seed s)
      Just _ -> V.AccFailure (InvalidSeconds str :| [])

parseMaybe
  :: Tx.Text
  -> (Tx.Text -> V.AccValidation (L.NE.NonEmpty ConfigError) a)
  -> [String]
  -> V.AccValidation (L.NE.NonEmpty ConfigError) (Maybe a)

parseMaybe name build args'
  = fromEither $
      case L.elemIndex (Tx.unpack name) args' of
        Nothing -> pure Nothing
        Just argIndex -> do
          argValue <- args' ^? element (argIndex + 1) `failIfNothing`
            NoValueForArgument name :| []

          fromValidation (Just <$> build (Tx.pack argValue))
parse
  :: Tx.Text
  -> (Tx.Text -> V.AccValidation (L.NE.NonEmpty ConfigError) a)
  -> [String]
  -> V.AccValidation (L.NE.NonEmpty ConfigError) a

parse name build args'
  = case parseMaybe name build args' of
       V.AccFailure err -> V.AccFailure err
       V.AccSuccess Nothing -> V.AccFailure (ArgumentWithNameDoesNotExist name :| [])
       V.AccSuccess (Just v) -> pure v

fromEither :: Semi.Semigroup e => Either e a -> V.AccValidation e a
fromEither ei
  = case ei of
      Left err -> V.AccFailure err
      Right v -> V.AccSuccess v

fromValidation :: Semi.Semigroup e => V.AccValidation e a -> Either e a
fromValidation v
  = case v of
      V.AccFailure err -> Left err
      V.AccSuccess v' -> Right v'

failIfNothing :: Maybe a -> err -> Either err a
failIfNothing mb err
  = case mb of
      Nothing -> Left err
      Just v -> Right v

infixl 1 `failIfNothing`

mkEndpoints
  :: File
  -> IO (V.AccValidation
      (L.NE.NonEmpty ConfigError) (NonEmptySet Endpoint))

mkEndpoints file@(File f)
  = do
      content <- S.IO.readFile (Tx.unpack f)
      case L.filter (not . null) (L.lines content) of
        [] -> pure (V.AccFailure (EmptyNodesConfig file :| []))
        n:ns ->
          let neL = traverse mkEndpoint (n !: ns)
          in pure (NE.mapTail Set.fromList <$> neL)

  where
    mkEndpoint str
      = case Tx.splitOn ":" (Tx.pack str) of
          [h, p] ->
            Mb.maybe
              (V.AccFailure (InvalidEndpoint (Tx.pack str) :| []))
              (pure . Endpoint h)
              (Tx.R.readMaybe (Tx.unpack p))

          _ -> V.AccFailure (InvalidEndpoint (Tx.pack str) :| [])

buildConfig
  :: V.AccValidation (L.NE.NonEmpty ConfigError)
       (Maybe File, Seconds, Seconds, Seed)

  -> IO (V.AccValidation (L.NE.NonEmpty ConfigError) Config)

buildConfig vf
  = case vf of
      V.AccFailure err -> pure (V.AccFailure err)
      V.AccSuccess (file, sf, wf, ws) ->
        let file' = Mb.fromMaybe (File "nodes.conf") file
            file'' = _fileText file'

        in do
             exists <- S.D.doesFileExist (Tx.unpack file'')
             endpoints <- if exists
               then mkEndpoints file'
               else pure (V.AccFailure (FileDoesNotExist file' :| []))

             case endpoints of
               V.AccFailure err -> pure (V.AccFailure err)
               V.AccSuccess e -> pure (V.AccSuccess (Config e sf wf ws))

