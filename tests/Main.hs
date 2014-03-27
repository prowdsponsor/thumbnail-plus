module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import System.Directory (doesFileExist)
import Test.Hspec

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Graphics.ThumbnailPlus as TP
import qualified Graphics.ThumbnailPlus.ImageSize as TPIS


jpn_art :: FilePath
jpn_art = "tests/data/jpn_art.jpg"


main :: IO ()
main = hspec $ do
  describe "sinkImageInfo" $ do
    it "works for logo.png"    $ check (Just (TP.Size  271   61, TP.PNG)) "tests/data/logo.png"
    it "works for logo.jpg"    $ check (Just (TP.Size  271   61, TP.JPG)) "tests/data/logo.jpg"
    it "works for logo.gif"    $ check (Just (TP.Size  271   61, TP.GIF)) "tests/data/logo.gif"
    it "works for jpg_art.jpg" $ check (Just (TP.Size 1344 1352, TP.JPG)) jpn_art
    it "rejects invalid file"  $ check Nothing "tests/Main.hs"
  describe "createThumbnails" $ do
    let conf = def { TP.maxFileSize      = 450239
                   , TP.maxImageSize     = TP.Size 1344 1352
                   , TP.reencodeOriginal = TP.NewFileFormat TP.GIF
                   , TP.thumbnailSizes   = [(TP.Size i i, Nothing) | i <- [3000, 512, 64]]
                   }
    it "works for jpg_art.jpg" $ do
      fps <- R.runResourceT $ do
        TP.CreatedThumbnails thumbs _ <- TP.createThumbnails conf jpn_art
        liftIO $ do
          map TP.thumbSize thumbs `shouldBe` [TP.maxImageSize conf, TP.Size 508 512, TP.Size 63 64]
          map TP.thumbFormat thumbs `shouldBe` [TP.GIF, TP.JPG, TP.JPG]
          mapM_ checkThumbnail thumbs
        return (map TP.thumbFp thumbs)
      forM_ fps $ \fp -> doesFileExist fp `shouldReturn` False
    it "rejects due to large file size" $ do
      let conf' = conf { TP.maxFileSize = TP.maxFileSize conf - 1 }
      R.runResourceT (TP.createThumbnails conf' jpn_art) `shouldReturn` TP.FileSizeTooLarge (TP.maxFileSize conf)
    it "rejects due to large image width" $ do
      -- I should use some lens :).
      let conf' = conf { TP.maxImageSize = TP.Size 1343 9999 }
      R.runResourceT (TP.createThumbnails conf' jpn_art) `shouldReturn` TP.ImageSizeTooLarge (TP.maxImageSize conf)
    it "rejects due to large image height" $ do
      -- I should use some lens :).
      let conf' = conf { TP.maxImageSize = TP.Size 9999 1351 }
      R.runResourceT (TP.createThumbnails conf' jpn_art) `shouldReturn` TP.ImageSizeTooLarge (TP.maxImageSize conf)

check :: Maybe (TP.Size, TP.FileFormat) -> FilePath -> Expectation
check ex fp = do
  size <- C.runResourceT $ CB.sourceFile fp C.$$ TPIS.sinkImageInfo
  size `shouldBe` ex

checkThumbnail :: TP.Thumbnail -> Expectation
checkThumbnail t = check (Just (TP.thumbSize t, TP.thumbFormat t)) (TP.thumbFp t)
