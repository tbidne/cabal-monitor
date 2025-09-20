{-# LANGUAGE TemplateHaskell #-}

module Cabal.Monitor.Args
  ( Args (..),
    Coloring (..),
    getArgs,
  )
where

import Cabal.Monitor.Args.TH qualified as TH
import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Version (showVersion)
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (HasCallStack)
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
    width :: Maybe Natural
  }
  deriving stock (Eq, Show)

newtype Coloring = MkColoring {unColoring :: Bool}
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
      Chunk.paragraph $
        mconcat
          [ "Cabal-monitor monitors a cabal output build file (e.g. cabal ",
            "build > out.txt) and reports basic progress information."
          ]

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> coloringParser
    <*> filePathParser
    <*> heightParser
    <*> periodParser
    <*> widthParser
      <**> OA.helper
      <**> version

filePathParser :: Parser OsPath
filePathParser =
  OA.option
    readPath
    $ mconcat
      [ OA.short 'f',
        OA.long "file",
        OA.metavar "PATH",
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
          OA.helpDoc helpTxt
        ]
  where
    readColoring =
      OA.str >>= \case
        "t" -> pure $ MkColoring True
        "true" -> pure $ MkColoring True
        "f" -> pure $ MkColoring False
        "false" -> pure $ MkColoring False
        bad -> fail $ "Unexpected --coloring: " ++ bad

    helpTxt =
      mconcat
        [ intro,
          Just Pretty.hardline,
          true,
          false,
          Just Pretty.hardline
        ]
    intro = toMDoc "Coloring options."
    true =
      mconcat
        [ Just Pretty.hardline,
          toMDoc "- (t|true): On."
        ]
    false =
      mconcat
        [ Just Pretty.hardline,
          toMDoc "- (f|false): Off."
        ]

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

widthParser :: Parser (Maybe Natural)
widthParser =
  OA.optional
    $ OA.option
      OA.auto
    $ mconcat
      [ OA.long "width",
        OA.metavar "NAT",
        mkHelp "Maximum line length."
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
      ghc = showVersion Info.compilerVersion,
      gitHash = h,
      gitShortHash = sh
    }
  where
    (d, h, sh) = $$TH.gitData

mkHelp :: String -> Mod f a
mkHelp s = mkMultiHelp [s]

mkMultiHelp :: [String] -> Mod f a
mkMultiHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.vsepChunks
    . fmap Chunk.paragraph

toMDoc :: String -> Maybe Doc
toMDoc = Chunk.unChunk . Chunk.paragraph
