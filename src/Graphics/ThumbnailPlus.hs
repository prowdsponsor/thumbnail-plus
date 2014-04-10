{-# LANGUAGE DeriveDataTypeable,
             RecordWildCards #-}
module Graphics.ThumbnailPlus
  ( createThumbnails
  , Configuration(..)
  , Size(..)
  , ReencodeOriginal(..)
  , FileFormat(..)
  , CreatedThumbnails(..)
  , Thumbnail(..)
  , NoShow(..)
  ) where

import Control.Arrow ((***))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (runEitherT, left, right)
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import qualified Control.Exception as E
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Typeable as T
import qualified Graphics.GD as GD
import qualified System.Directory as D
import qualified System.IO as IO
import qualified System.IO.Temp as T

import Graphics.ThumbnailPlus.ImageSize


-- | Configuration used when
data Configuration =
  Configuration
    { maxFileSize :: !Integer
      -- ^ Maximum file size in bytes.  Files larger than this
      -- limit are rejected.  Default: 5 MiB.
    , maxImageSize :: !Size
      -- ^ Maximum image size in pixels.  Images which exceed this
      -- limit in any dimension are rejected.  Default: 3000x3000px.
    , reencodeOriginal :: !ReencodeOriginal
      -- ^ Whether the original image should be reencoded.
      -- Default: 'SameFileFormat'.
    , thumbnailSizes :: [(Size, Maybe FileFormat)]
      -- ^ The sizes of the thumbnails that should be created.
      -- Thumbnails preserve the aspect ratio and have at least
      -- one dimension equal to the given requested size.  Sizes
      -- larger than the original image will be disregarded. If a
      -- 'FileFormat' is not provided, the same file format as
      -- the original image is reused.  Default: 512x512, 64x64.
    , temporaryDirectory :: IO FilePath
      -- ^ Temporary directory where files should be
      -- saved. Default: 'D.getTemporaryDirectory'.
    } deriving (T.Typeable)

instance Default Configuration where
  def = Configuration
          { maxFileSize        = 2 * 1024 * 1024
          , maxImageSize       = Size 4096 4096
          , reencodeOriginal   = SameFileFormat
          , thumbnailSizes     = [(Size 512 512, Nothing), (Size 64 64, Nothing)]
          , temporaryDirectory = D.getTemporaryDirectory
          }

-- | Whether the original image should be reencoded or not (cf.,
-- 'reencodeOriginal').
data ReencodeOriginal =
    Never
    -- ^ Do not reencode the original image.
  | SameFileFormat
    -- ^ Reencode the original using the same file format.
  | NewFileFormat !FileFormat
    -- ^ Reencode the original using the given file format.
    deriving (Eq, Ord, Show, T.Typeable)


-- | cf. 'thumbnailSizes'.
calculateThumbnailSize
  :: Size -- ^ Original size.
  -> Size -- ^ Thumbnail max size.
  -> Size -- ^ Calculated thumbnail size.
calculateThumbnailSize (Size ow oh) (Size tw th) =
  let -- Assuming final height is th.
      wByH = ow * th `div` oh
      -- Assuming final width is tw.
      hByW = oh * tw `div` ow
  in if wByH > tw
     then Size tw hByW
     else Size wByH th


-- | Process an image and generate thumbnails for it according to
-- the given 'Configuration'.
createThumbnails
  :: R.MonadResource m
  => Configuration -- ^ Configuration values (use 'def' for default values).
  -> FilePath      -- ^ Input image file path.
  -> m CreatedThumbnails
createThumbnails conf inputFp = do
  checkRet <- liftIO (checkInput conf inputFp)
  case checkRet of
    Left ret         -> return ret
    Right (size, ff) -> doCreateThumbnails conf (inputFp, size, ff)

checkInput :: Configuration -> FilePath -> IO (Either CreatedThumbnails (Size, FileFormat))
checkInput Configuration {..} inputFp =
  -- No resources from the input check are needed to create the
  -- thumbnails, use an inner ResourceT.
  R.runResourceT $ runEitherT $ do
    (_, inputH) <- lift $ R.allocate (IO.openFile inputFp IO.ReadMode) IO.hClose
    fileSize <- liftIO $ IO.hFileSize inputH
    unless (fileSize <= maxFileSize) $ left (FileSizeTooLarge fileSize)
    minfo <- CB.sourceHandle inputH C.$$ sinkImageInfo
    info@(imageSize, _) <- maybe (left ImageFormatUnrecognized) right minfo
    unless (imageSize `fits` maxImageSize) $ left (ImageSizeTooLarge imageSize)
    return info

fits :: Size -> Size -> Bool
Size aw ah `fits` Size bw bh = aw <= bw && ah <= bh

doCreateThumbnails
  :: R.MonadResource m
  => Configuration
  -> (FilePath, Size, FileFormat)
  -> m CreatedThumbnails
doCreateThumbnails Configuration {..} (inputFp, inputSize, inputFf) = do
  parentDir <- liftIO temporaryDirectory
  (relTmpDir, tmpDir) <-
    R.allocate
      (T.createTempDirectory parentDir "thumbnail-plus-")
      (ignoringIOErrors . D.removeDirectoryRecursive)
  (relImg, img) <-
    R.allocate
      (($ inputFp) $ case inputFf of
                       GIF -> GD.loadGifFile
                       JPG -> GD.loadJpegFile
                       PNG -> GD.loadPngFile)
      gdFreeImage
  imgSize <- liftIO $ do
    GD.alphaBlending True img
    GD.imageSize img
  case (imgSize, inputSize) of
    ((w1,h1), Size w2 h2) | w1 /= w2 || h1 /= h2 ->
      -- Sanity check
      return ImageFormatUnrecognized
    _ -> do
      let finalThumbSizes =
            (case reencodeOriginal of
               Never            -> id
               SameFileFormat   -> (:) (inputSize, inputFf)
               NewFileFormat ff -> (:) (inputSize, ff)) $
            map (calculateThumbnailSize inputSize *** fromMaybe inputFf) $
            filter (not . (inputSize `fits`) . fst) $
            thumbnailSizes
      thumbnails <- mapM (createThumbnail tmpDir (img, imgSize)) finalThumbSizes
      R.release relImg
      return (CreatedThumbnails thumbnails (NoShow relTmpDir))

createThumbnail
  :: R.MonadResource m
  => FilePath
  -> (GD.Image, GD.Size)
  -> (Size, FileFormat)
  -> m Thumbnail
createThumbnail tmpDir (inputImg, inputImgSize) (size@(Size w h), ff) = do
  let template = "thumb-" ++ show w ++ "x" ++ show h ++ "-"
  (relTmpFile, (tmpFp, tmpHandle)) <-
    R.allocate
      (T.openBinaryTempFile tmpDir template)
      (\(tmpFile, tmpHandle) ->
        ignoringIOErrors $ do
          IO.hClose tmpHandle
          D.removeFile tmpFile)
  liftIO $ do
    -- The @gd@ library does not support 'Handle's :'(, close it as
    -- early as possible.  We don't close it above on the
    -- 'R.allocate' because of async exceptions, but it doesn't
    -- matter since hClose-ing twice is harmless.
    IO.hClose tmpHandle
    GD.withImage (GD.newImage (w, h)) $ \resizedImg -> do
      GD.alphaBlending False resizedImg
      GD.saveAlpha     True  resizedImg
      GD.copyRegionScaled (0, 0) inputImgSize inputImg
                          (0, 0) (w, h)       resizedImg
      (\f -> f tmpFp resizedImg) $
        case ff of
          GIF -> GD.saveGifFile
          JPG -> GD.saveJpegFile (-1) -- TODO: Configurable JPEG quality.
          PNG -> GD.savePngFile
  return Thumbnail { thumbFp         = tmpFp
                   , thumbSize       = size
                   , thumbFormat     = ff
                   , thumbReleaseKey = NoShow relTmpFile
                   }

-- | For some reason, the @gd@ library does not export
-- 'GD.freeImage'.  Argh!
gdFreeImage :: GD.Image -> IO ()
gdFreeImage img = GD.withImage (return img) (const $ return ())

-- | Shamelessly copied and adapted from @temporary@ package.
ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `E.catch` (\e -> const (return ()) (e :: IOError))


-- | Return value of 'createThumbnails'.
data CreatedThumbnails =
    FileSizeTooLarge !Integer
    -- ^ File size exceeded 'maxFileSize'.
  | ImageSizeTooLarge !Size
    -- ^ Image size exceeded 'maxImageSize'.
  | ImageFormatUnrecognized
    -- ^ Could not parse size information for the image.
    -- Remember that we understand JPGs, PNGs and GIFs only.
  | CreatedThumbnails ![Thumbnail] !(NoShow R.ReleaseKey)
    -- ^ Thumbnails were created successfully.  If
    -- 'reencodeOriginal' was not 'Never', then the first item of
    -- the list is going to be the reencoded image.
  deriving (Eq, Show, T.Typeable)


-- | Information about a generated thumbnail.  Note that if ask
-- for the original image to be reencoded, then the first
-- 'Thumbnail' will actually have the size of the original image.
data Thumbnail =
  Thumbnail
    { thumbFp :: !FilePath
      -- ^ The 'FilePath' where this thumbnail is stored.
    , thumbSize :: !Size
      -- ^ Size of the thumbnail.
    , thumbFormat :: !FileFormat
    , thumbReleaseKey :: !(NoShow R.ReleaseKey)
      -- ^ Release key that may be used to clean up any resources
      -- used by this thumbnail as soon as possible (i.e., before
      -- 'R.runResourceT' finishes).
    } deriving (Eq, Show, T.Typeable)


-- | Hack to allow me to derive 'Show' for data types with fields
-- that don't have 'Show' instances.
newtype NoShow a = NoShow a

instance T.Typeable a => Show (NoShow a) where
  showsPrec _ ~(NoShow a) =
    ('<':) . T.showsTypeRep (T.typeOf a) . ('>':)

instance Eq (NoShow a) where
  _ == _ = True
