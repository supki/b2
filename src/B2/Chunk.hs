-- Chunking with a raw buffer
module B2.Chunk 
  ( chunkBySize
  ) where

import           Control.Monad.IO.Class        (MonadIO(..))
import           Data.ByteString               (ByteString)
import           Data.Word                     (Word8)
import           Data.Conduit                  (ConduitT, await, yield)
import           Foreign.ForeignPtr            (ForeignPtr)
import           Foreign.ForeignPtr.Unsafe     (unsafeForeignPtrToPtr)
import           Foreign.Ptr                   (Ptr)
import qualified Data.ByteString as B
import           Data.ByteString.Internal      (ByteString (PS), mallocByteString)
import           Data.ByteString.Unsafe        (unsafeIndex)
import           Foreign.Storable              (pokeByteOff)

type ChunkSize = Int

data S = S (ForeignPtr Word8) (Ptr Word8) {-# UNPACK #-} !Int

newS :: ChunkSize -> IO S
newS chunkSize = do
    fptr <- mallocByteString chunkSize
    return (S fptr (unsafeForeignPtrToPtr fptr) 0)

processChunk :: ChunkSize -> ByteString -> S -> IO ([ByteString], S)
processChunk chunkSize input =
    loop id 0
  where
    loop front idxIn s@(S fptr ptr idxOut)
        | idxIn >= B.length input = return (front [], s)
        | otherwise = do
            pokeByteOff ptr idxOut (unsafeIndex input idxIn)
            let idxOut' = idxOut + 1
                idxIn' = idxIn + 1
            if idxOut' >= chunkSize
                then do
                    let bs = PS fptr 0 idxOut'
                    s' <- newS chunkSize
                    loop (front . (bs:)) idxIn' s'
                else loop front idxIn' (S fptr ptr idxOut')

chunkBySize :: MonadIO m => ChunkSize -> ConduitT ByteString ByteString m ()
chunkBySize chunkSize =
    liftIO (newS chunkSize) >>= loop
  where
    loop s@(S fptr _ len) = do
        mbs <- await
        case mbs of
            Nothing -> yield $ PS fptr 0 len
            Just bs -> do
                (bss, s') <- liftIO $ processChunk chunkSize bs s
                mapM_ yield bss
                loop s'