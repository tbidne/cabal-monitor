{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Cabal.Monitor.Args
  ( Args (..),
    Coloring (..),
    SearchInfix (..),
    getArgs,
  )
where

import Cabal.Monitor.Args.TH qualified as TH
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString (fromString))
import Data.Version (showVersion)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (HasCallStack)
import Effectful.Optparse.Completer qualified as EOC
import Effectful.Optparse.Static (Optparse)
import Effectful.Optparse.Static qualified as EOA
import FileSystem.OsPath (OsPath)
import FileSystem.OsPath qualified as OsPath
import FileSystem.OsString (OsString)
import FileSystem.OsString qualified as OsString
import Numeric.Natural (Natural)
import Options.Applicative
  ( Mod,
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help (Chunk (Chunk), Doc)
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_cabal_monitor qualified as Paths
import System.Info qualified as Info

-- | CLI args.
data Args = MkArgs
  { coloring :: Maybe Coloring,
    filePath :: OsPath,
    height :: Maybe Natural,
    period :: Maybe Natural,
    searchInfix :: Maybe SearchInfix,
    width :: Maybe Natural
  }
  deriving stock (Eq, Show)

newtype Coloring = MkColoring {unColoring :: Bool}
  deriving stock (Eq, Show)

newtype SearchInfix = MkSearchInfix {unSearchInfix :: Bool}
  deriving stock (Eq, Show)

getArgs :: (HasCallStack, Optparse :> es) => Eff es Args
getArgs = EOA.execParser parserInfo

-- | Optparse-Applicative info.
parserInfo :: ParserInfo Args
parserInfo =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "Cabal-monitor: Monitors cabal builds"
    footerTxt = Just $ fromString versShort
    desc =
      Chunk.vcatChunks
        [ Chunk.paragraph $
            mconcat
              [ "Cabal-monitor monitors a cabal output build file (e.g. cabal ",
                "build > out.txt) and reports basic progress information."
              ],
          line,
          Chunk.paragraph "Examples:",
          line,
          mkExample
            [ "1. Simple usage:",
              "",
              "In terminal 1",
              "$ cabal build > out.txt ",
              "",
              "In terminal 2",
              "$ cabal-monitor -f out.txt "
            ]
        ]

    mkExample :: NonEmpty String -> Chunk Doc
    mkExample = identPara 2 5

    identPara :: Int -> Int -> NonEmpty String -> Chunk Doc
    identPara hIndent lIndent (h :| xs) =
      Chunk.vcatChunks
        . (\ys -> toChunk hIndent h : ys)
        . fmap (toChunk lIndent)
        $ xs

    toChunk _ "" = line
    toChunk i other = fmap (Pretty.indent i) . Chunk.stringChunk $ other

    line = Chunk (Just Pretty.softline)

argsParser :: Parser Args
argsParser = mainParser <**> OA.helper <**> version
  where
    mainParser = do
      ~(filePath, period, searchInfix) <- coreOptsParser
      ~(coloring, height, width) <- formattingOptsParser

      pure $
        MkArgs
          { coloring,
            filePath,
            height,
            period,
            searchInfix,
            width
          }

    coreOptsParser =
      OA.parserOptionGroup
        "Core options:"
        $ (,,)
          <$> filePathParser
          <*> periodParser
          <*> searchInfixParser

    formattingOptsParser =
      OA.parserOptionGroup
        "Formatting options:"
        $ (,,)
          <$> coloringParser
          <*> heightParser
          <*> widthParser

filePathParser :: Parser OsPath
filePathParser =
  OA.option
    readPath
    $ mconcat
      [ OA.short 'f',
        OA.long "file",
        OA.metavar "PATH",
        OA.completer EOC.compgenCwdPathsCompleter,
        mkHelp "Path to file to monitor."
      ]
  where
    readPath = OA.str >>= OsPath.encodeFail

heightParser :: Parser (Maybe Natural)
heightParser =
  OA.optional
    $ OA.option
      OA.auto
    $ mconcat
      [ OA.long "height",
        OA.metavar "NAT",
        mkHelp "Maximum number of lines to display."
      ]

coloringParser :: Parser (Maybe Coloring)
coloringParser =
  OA.optional $
    OA.option readColoring $
      mconcat
        [ OA.long "color",
          OA.metavar "(on | off)",
          OA.completeWith ["on", "off"],
          mkHelp "Coloring options. Defaults to 'on'."
        ]
  where
    readColoring =
      OA.str >>= \case
        "on" -> pure $ MkColoring True
        "off" -> pure $ MkColoring False
        bad -> fail $ "Unexpected --coloring: " ++ bad

periodParser :: Parser (Maybe Natural)
periodParser =
  OA.optional
    $ OA.option
      OA.auto
    $ mconcat
      [ OA.short 'p',
        OA.long "period",
        OA.metavar "NAT",
        mkHelp "Monitor refresh period, in seconds."
      ]

searchInfixParser :: Parser (Maybe SearchInfix)
searchInfixParser =
  OA.optional $
    OA.option readFn $
      mconcat
        [ OA.long "search-infix",
          OA.metavar "(on | off)",
          OA.completeWith ["on", "off"],
          mkHelpNoLine helpTxt
        ]
  where
    readFn =
      OA.str >>= \case
        "on" -> pure $ MkSearchInfix True
        "off" -> pure $ MkSearchInfix False
        bad -> fail $ "Unexpected --search-infix: " ++ bad

    helpTxt =
      mconcat
        [ "Searches for expected cabal log keywords as infix patterns, as ",
          "opposed to prefix. Slower but more flexible e.g. compatible with ",
          "the cabal log file being processed to have each line prefixed ",
          "with a timestamp. Defaults to 'off'."
        ]

widthParser :: Parser (Maybe Natural)
widthParser =
  OA.optional
    $ OA.option
      OA.auto
    $ mconcat
      [ OA.long "width",
        OA.metavar "NAT",
        mkHelpNoLine "Maximum line length."
      ]

version :: Parser (a -> a)
version = OA.infoOption versLong (OA.long "version" <> OA.short 'v' <> OA.hidden)

versShort :: String
versShort =
  mconcat
    [ "Version: ",
      showVersion Paths.version,
      " (",
      OsString.decodeLenient versionInfo.gitShortHash,
      ")"
    ]

versLong :: String
versLong =
  L.intercalate
    "\n"
    [ "Cabal-monitor: " <> showVersion Paths.version,
      " - Git revision: " <> OsString.decodeLenient versionInfo.gitHash,
      " - Commit date:  " <> OsString.decodeLenient versionInfo.gitCommitDate,
      " - GHC version:  " <> versionInfo.ghc
    ]

data VersionInfo = MkVersionInfo
  { gitCommitDate :: OsString,
    ghc :: String,
    gitHash :: OsString,
    gitShortHash :: OsString
  }

versionInfo :: VersionInfo
versionInfo =
  MkVersionInfo
    { gitCommitDate = d,
      ghc = showVersion Info.fullCompilerVersion,
      gitHash = h,
      gitShortHash = sh
    }
  where
    (d, h, sh) = $$TH.gitData

mkHelp :: String -> Mod f a
mkHelp s = mkMultiHelp [s]

mkHelpNoLine :: String -> Mod f a
mkHelpNoLine s = mkMultiHelpNoLine [s]

mkMultiHelp :: [String] -> Mod f a
mkMultiHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.vsepChunks
    . fmap Chunk.paragraph

mkMultiHelpNoLine :: [String] -> Mod f a
mkMultiHelpNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . Chunk.vsepChunks
    . fmap Chunk.paragraph

toMDoc :: String -> Maybe Doc
toMDoc = Chunk.unChunk . Chunk.paragraph
