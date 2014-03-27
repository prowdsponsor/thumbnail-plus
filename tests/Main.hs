module Main (main) where

import Test.Hspec

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Graphics.ThumbnailPlus as TP
import qualified Graphics.ThumbnailPlus.ImageSize as TPIS


main :: IO ()
main = hspec $ do
  describe "sinkImageInfo" $ do
    it "works for logo.png" $ check (Just (TP.Size 271 61, TP.PNG)) "tests/data/logo.png"
    it "works for logo.jpg" $ check (Just (TP.Size 271 61, TP.JPG)) "tests/data/logo.jpg"
    it "works for logo.gif" $ check (Just (TP.Size 271 61, TP.GIF)) "tests/data/logo.gif"
    it "rejects invalid file" $ check Nothing "tests/Main.hs"

check :: Maybe (TP.Size, TP.FileFormat) -> FilePath -> Expectation
check ex fp = do
  size <- C.runResourceT $ CB.sourceFile fp C.$$ TPIS.sinkImageInfo
  size `shouldBe` ex
