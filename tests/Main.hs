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


main :: IO ()
main = hspec $ do
  describe "sinkImageInfo" $ do
    it "works for logo.png"    $ check (Just (TP.Size  271   61, TP.PNG)) "tests/data/logo.png"
    it "works for logo.jpg"    $ check (Just (TP.Size  271   61, TP.JPG)) "tests/data/logo.jpg"
    it "works for logo.gif"    $ check (Just (TP.Size  271   61, TP.GIF)) "tests/data/logo.gif"
    it "works for jpg_art.jpg" $ check (Just (TP.Size 1344 1352, TP.JPG)) "tests/data/jpn_art.jpg"
    it "rejects invalid file"  $ check Nothing "tests/Main.hs"
  describe "createThumbnails" $ do
    it "works for jpg_art.jpg" $ do
      let conf = def { TP.reencodeOriginal = TP.NewFileFormat TP.GIF
                     , TP.thumbnailSizes   = [(TP.Size i i, Nothing) | i <- [3000, 512, 64]]
                     }
      fps <- R.runResourceT $ do
        TP.CreatedThumbnails thumbs _ <- TP.createThumbnails conf "tests/data/jpn_art.jpg"
        liftIO $ do
          map TP.thumbSize thumbs `shouldBe` [TP.Size 1344 1352, TP.Size 508 512, TP.Size 63 64]
          map TP.thumbFormat thumbs `shouldBe` [TP.GIF, TP.JPG, TP.JPG]
          mapM_ checkThumbnail thumbs
        return (map TP.thumbFp thumbs)
      forM_ fps $ \fp -> doesFileExist fp `shouldReturn` False

check :: Maybe (TP.Size, TP.FileFormat) -> FilePath -> Expectation
check ex fp = do
  size <- C.runResourceT $ CB.sourceFile fp C.$$ TPIS.sinkImageInfo
  size `shouldBe` ex

checkThumbnail :: TP.Thumbnail -> Expectation
checkThumbnail t = check (Just (TP.thumbSize t, TP.thumbFormat t)) (TP.thumbFp t)
