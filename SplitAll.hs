{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}


module Main where


import           Control.Monad
import qualified Data.List                 as L
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS hiding ((</>))
import           Prelude                   hiding (FilePath)
import           Shelly

default (T.Text)


toBoxDir :: FilePath
toBoxDir = "/Volumes/Untitled/to-box"

splitSize :: T.Text
splitSize = "4096m"

-- | This should be something smaller than 4m, which is above.
maxWholeSize :: Integer
maxWholeSize = 3 * 2^(30 :: Integer)

main :: IO ()
main = shelly $ verbosely $ chdir toBoxDir $
        ls toBoxDir
    >>= filterM (liftIO . fmap (>= maxWholeSize) . FS.getSize)
    .   filter (hasExt "xz")
    >>= mapM_ splitFile . L.sort

splitFile :: FilePath -> Sh ()
splitFile fn = do
    mkdir_p name
    chdir name . split $ (".." :: FilePath) </> fn
    cleanUp fn name
    where name = basename fn

split :: FilePath -> Sh ()
split fn = run_ "split" ["-b", splitSize, toTextIgnore fn]

cleanUp :: FilePath -> FilePath -> Sh ()
cleanUp fn dn = cleanUp' =<< ls dn
    where cleanUp' [_] =  echo ("One split. Removing " <> toTextIgnore dn)
                       >> rm_rf dn
          cleanUp' _   =  echo (toTextIgnore fn <> " split. Removing.")
                       >> rm fn
