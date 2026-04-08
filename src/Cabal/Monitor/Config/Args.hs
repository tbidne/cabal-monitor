{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Cabal.Monitor.Config.Args
  ( -- * Args
    Args (..),
    getArgs,
  )
where

import Cabal.Monitor.Config.Args.TH qualified as TH
import Cabal.Monitor.Config.Data
  ( Coloring (MkColoring),
    Debug (MkDebug),
    Height (MkHeight),
    LocalPackages (MkLocalPackages),
    Period (MkPeriod),
    Pid (MkPid),
    SearchInfix (MkSearchInfix),
    Width (MkWidth),
  )
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
import Options.Applicative
  ( Mod,
    OptionFields,
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
    ReadM,
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
  { -- | Whether to color the logs.
    coloring :: Maybe Coloring,
    -- | Path to toml config.
    configPath :: Maybe OsPath,
    -- | Debug flag.
    debug :: Maybe Debug,
    -- | Path to file to monitor.
    filePath :: OsPath,
    -- | Possible terminal height.
    height :: Maybe Height,
    -- | Whether to monitor output for local packages (requires extra logic).
    localPackages :: Maybe LocalPackages,
    -- | How often to read the status, in seconds.
    period :: Maybe Period,
    -- | Pid of the process we are monitoring, for exiting automatically.
    pid :: Maybe Pid,
    -- | Whether to search logs for infix patterns, for more flexibility at
    -- the cost of performance.
    searchInfix :: Maybe SearchInfix,
    -- | Possible terminal width.
    width :: Maybe Width
  }
  deriving stock (Eq, Show)

-- | Reads CLI args.
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
      ~(configPath, debug, filePath, localPackages, period, pid, searchInfix) <- coreOptsParser
      ~(coloring, height, width) <- formattingOptsParser

      pure $
        MkArgs
          { coloring,
            configPath,
            debug,
            filePath,
            height,
            localPackages,
            period,
            pid,
            searchInfix,
            width
          }

    coreOptsParser =
      OA.parserOptionGroup
        "Core options:"
        $ (,,,,,,)
          <$> configParser
          <*> debugParser
          <*> filePathParser
          <*> localPackagesParser
          <*> periodParser
          <*> pidParser
          <*> searchInfixParser

    formattingOptsParser =
      OA.parserOptionGroup
        "Formatting options:"
        $ (,,)
          <$> coloringParser
          <*> heightParser
          <*> widthParser

configParser :: Parser (Maybe OsPath)
configParser =
  OA.optional
    $ OA.option
      readPath
    $ mconcat
      [ OA.short 'c',
        OA.long "config",
        OA.metavar "(PATH | off)",
        OA.completer EOC.compgenCwdPathsCompleter,
        mkHelp $
          mconcat
            [ "Path to TOML config file. If not given, we automatically look ",
              "in XDG config e.g. ~/.config/cabal-monitor/config.toml"
            ]
      ]
  where
    readPath = OA.str >>= OsPath.encodeFail

debugParser :: Parser (Maybe Debug)
debugParser =
  OA.optional
    . fmap MkDebug
    . mkSwitch
    $ mconcat
      [ OA.long "debug",
        OA.internal
      ]

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

heightParser :: Parser (Maybe Height)
heightParser =
  OA.optional
    $ OA.option
      (MkHeight <$> OA.auto)
    $ mconcat
      [ OA.long "height",
        OA.metavar "NAT",
        mkHelp "Maximum number of lines to display."
      ]

pidParser :: Parser (Maybe Pid)
pidParser =
  OA.optional
    $ OA.option
      (MkPid <$> OA.auto)
    $ mconcat
      [ OA.long "pid",
        OA.metavar "NAT",
        mkHelp $
          mconcat
            [ "The pid of the process to watch (usually cabal). Used to exit ",
              "automatically after the process has finished."
            ]
      ]

coloringParser :: Parser (Maybe Coloring)
coloringParser =
  OA.optional
    . fmap MkColoring
    . mkSwitch
    $ mconcat
      [ OA.long "color",
        mkHelp "Coloring options. Defaults to 'on'."
      ]

localPackagesParser :: Parser (Maybe LocalPackages)
localPackagesParser =
  OA.optional
    . fmap MkLocalPackages
    . mkSwitch
    $ mconcat
      [ OA.long "local-packages",
        mkHelp $
          mconcat
            [ "Local packages require special handling in order to detect ",
              "completion. This flag turns this handling on, at a significant ",
              "performance cost. Defaults to 'on'."
            ]
      ]

periodParser :: Parser (Maybe Period)
periodParser =
  OA.optional
    $ OA.option
      (MkPeriod <$> OA.auto)
    $ mconcat
      [ OA.long "period",
        OA.metavar "NAT",
        mkHelp "Monitor refresh period, in seconds."
      ]

searchInfixParser :: Parser (Maybe SearchInfix)
searchInfixParser =
  OA.optional
    . fmap MkSearchInfix
    . mkSwitch
    $ mconcat
      [ OA.long "search-infix",
        mkHelp $
          mconcat
            [ "Searches for expected cabal log keywords as infix patterns, as ",
              "opposed to prefix. Slower but more flexible e.g. compatible with ",
              "the cabal log file being processed to have each line prefixed ",
              "with a timestamp. Defaults to 'off'."
            ]
      ]

widthParser :: Parser (Maybe Width)
widthParser =
  OA.optional
    $ OA.option
      (MkWidth <$> OA.auto)
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

-- Makes a switch that takes '(on | off)'. For consistency, this should be
-- preferred for any on/off switch, rather than a normal flag
-- (e.g. --foo (on | off) vs. --foo).
mkSwitch :: Mod OptionFields Bool -> Parser Bool
mkSwitch opts = OA.option readSwitch opts'
  where
    opts' =
      OA.metavar "(on | off)"
        <> OA.completeWith ["on", "off"]
        <> opts

readSwitch :: ReadM Bool
readSwitch =
  OA.str >>= \case
    "off" -> pure False
    "on" -> pure True
    other -> fail $ "Expected (on | off), received: " ++ other
