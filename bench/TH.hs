{-# LANGUAGE QuasiQuotes #-}

module TH
  ( readSampleTH,
  )
where

import Data.ByteString (ByteString)
import FileSystem.IO qualified as IO
import FileSystem.OsPath (ospPathSep)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (liftTyped))

readSampleTH :: Code Q ByteString
readSampleTH =
  liftIOToTH $ IO.readBinaryFileIO [ospPathSep|./bench/sample.txt|]

-- | Binds an IO action to TH.
bindIOToTH :: (HasCallStack, Lift b) => ((HasCallStack) => a -> IO b) -> a -> Code Q b
bindIOToTH f x = TH.bindCode (TH.runIO (f x)) liftTyped

-- | Lifts an IO action to TH.
liftIOToTH :: (HasCallStack, Lift a) => IO a -> Code Q a
liftIOToTH m = bindIOToTH (const m) ()
