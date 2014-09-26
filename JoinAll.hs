{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}


module Main where


import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy      as BS
import qualified Data.List                 as L
import           Data.Monoid
import qualified Data.Text                 as T
import           Filesystem
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS hiding ((<.>), (</>))
import           Prelude                   hiding (FilePath)
import           Shelly

default (T.Text)


toBoxDir :: FilePath
toBoxDir = "/Volumes/Untitled/to-box"


main :: IO ()
main = shelly $ verbosely $ chdir toBoxDir $
    mapM_ joinFile . L.sort =<< filterM (liftIO . isDirectory) =<< ls toBoxDir


joinFile :: FilePath -> Sh ()
joinFile dir = do
    let output = dir <.> "tar.xz"
    echo $ "joining " <> toTextIgnore dir <> " => " <> toTextIgnore output

    xs <- L.sort <$> ls dir
    liftIO . forM_ xs $ \x ->  BS.readFile   (encodeString x)
                           >>= BS.appendFile (encodeString output)

    rm_rf dir
