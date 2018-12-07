#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.21
  --ghc-options -Wall
  --package text
  --package vector
-}

{-# LANGUAGE OverloadedStrings #-}

import Prelude (IO, Show, (+), (-), (*), (.), ($), (==), (/=), (>=), (||), error, fromIntegral, return, show)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Bits ((.|.))
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.IO (putStrLn, readFile, writeFile)
import Data.Text.Foreign (asForeignPtr, fromPtr, lengthWord16, unsafeCopyToPtr)
import Data.Vector.Storable (Vector, freeze, length)
import Data.Vector.Storable.Mutable (grow, replicate, unsafeFromForeignPtr0, unsafeWith, write)
import Data.Word (Word16, Word8)
import Foreign (Ptr, castPtr, nullPtr, peek, plusPtr, ptrToIntPtr)
import Foreign.C.Types (CUChar(..), CInt(..), CUInt(..), CUShort(..))
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (Storable, alignment, poke, pokeByteOff, sizeOf)
import qualified Data.Vector.Storable

-- foreign imports
-- https://ghc.haskell.org/trac/ghc/ticket/12890

foreign import ccall safe "CreateFileW"
    createFileW :: Ptr Word16 -> CUInt -> CUInt -> Ptr () -> CUInt -> CUInt -> Ptr () -> IO (Ptr ())

foreign import ccall safe "DeleteFileW"
    deleteFileW :: Ptr Word16 -> IO CInt

foreign import ccall safe "CreateDirectoryW"
    createDirectoryW :: Ptr Word16 -> Ptr () -> IO CInt

foreign import ccall safe "RemoveDirectoryW"
    removeDirectoryW :: Ptr Word16 -> IO CInt

foreign import ccall unsafe "GetLastError"
    getLastError :: IO CUInt

foreign import ccall safe "FormatMessageW"
    formatMessageW :: CUInt -> Ptr () -> CUInt -> CUInt -> Ptr (Ptr Word16) -> CUInt -> Ptr () -> IO CUInt

foreign import ccall unsafe "LocalFree"
    localFree :: Ptr a -> IO ()

foreign import ccall safe "CloseHandle"
    closeHandle :: Ptr () -> IO CInt

foreign import ccall safe "DeviceIoControl"
    deviceIoControl :: Ptr () -> CUInt -> Ptr () -> CUInt -> Ptr () -> CUInt -> Ptr () -> Ptr () -> IO CInt

foreign import ccall safe "GetCurrentDirectoryW"
    getCurrentDirectoryW :: CUInt -> Ptr Word16 -> IO CInt

foreign import ccall safe "CreateSymbolicLinkW"
    createSymbolicLinkW :: Ptr Word16 -> Ptr Word16 -> CUInt -> IO CUChar

-- helpers

workDir :: Text
workDir = "work"

maxPath :: CUInt
maxPath = 260

frozenUnsafeWith :: Storable a => Vector a -> (Ptr a -> IO b) -> IO b 
frozenUnsafeWith = Data.Vector.Storable.unsafeWith

withLPCWSTR :: Text -> (Ptr Word16 -> IO a) -> IO a
withLPCWSTR text fun = do
    (fptr, tlen) <- asForeignPtr text
    let len = fromIntegral tlen
    let vecInit = unsafeFromForeignPtr0 fptr len
    vec <- grow vecInit 1
    write vec len 0
    unsafeWith vec fun

-- FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS
formatFlags :: CUInt
formatFlags = 0x00000100 .|. 0x00001000 .|. 0x00000200

textError :: Text -> CUInt -> IO Text
textError call code = do
    vec <- replicate 1 (nullPtr :: Ptr Word16)
    unsafeWith vec $ \buf -> do
        len <- formatMessageW formatFlags nullPtr code 0 buf 0 nullPtr
        if 0 /= len then
            finally
                (do 
                    let msgLen = fromIntegral (len - 2)
                    ptr <- peek buf
                    msg <- fromPtr ptr msgLen
                    return $ "Error,"
                        <> " call: [" <> call <> "],"
                        <> " code: [" <> ((pack . show) code) <> "],"
                        <> " message: [" <> msg <> "]" )
                (do
                    ptr <- peek buf
                    localFree ptr)
        else do
            fc <- getLastError
            return $ "Formating error: [" <> ((pack . show) fc) <> "]"

throwLastError :: Text -> IO ()
throwLastError call = do
    code <- getLastError
    msg <- textError call code
    (error . unpack) msg

getCwd :: IO Text
getCwd = do
    buf <- replicate (fromIntegral maxPath) (0 :: Word16)    
    unsafeWith buf $ \ptr -> do
        let mlen = fromIntegral (maxPath - 1)
        len <- getCurrentDirectoryW mlen ptr
        when (0 == len || (fromIntegral len) >= mlen) $ do throwLastError "GetCurrentDirectory"
        fromPtr ptr (fromIntegral len) 

-- samples

-- http://www.flexhex.com/docs/articles/hard-links.phtml
-- sizeof == 20
data ReparsePointBuf = ReparsePointBuf
    { reparseTag :: CUInt
    , reparseDataLength :: CUInt
    , reserved :: CUShort
    , reparseTargetLength :: CUShort
    , reparseTargetMaximumLength :: CUShort
    , reserved1 :: CUShort
    , reparseTarget :: Vector Word16
    } deriving (Show)
instance Storable ReparsePointBuf where
    sizeOf val = 4 + 4 + 2 + 2 + 2 + 2 + (length (reparseTarget val) * 2)
    alignment _ = 4
    peek _ = error "Not supported"
    poke ptr val = do
        _ <- pokeByteOff ptr 0 (reparseTag val)
        _ <- pokeByteOff ptr 4 (reparseDataLength val)
        _ <- pokeByteOff ptr 8 (reserved val)
        _ <- pokeByteOff ptr 10 (reparseTargetLength val)
        _ <- pokeByteOff ptr 12 (reparseTargetMaximumLength val)
        _ <- pokeByteOff ptr 14 (reserved1 val)
        _ <- frozenUnsafeWith (reparseTarget val) $ \buf -> do
            let dest = plusPtr ptr 16
            let len = length (reparseTarget val) * 2
            copyBytes dest buf len
        return ()

junctionSample :: IO ()
junctionSample = do
    putStrLn "Running junction sample ..."
    let junctionTarget = workDir <> "\\junction_target"
    let junctionDir = workDir <> "\\junction"

    -- create junction dirs
    errCreateDir <- withLPCWSTR junctionTarget $ \txPtr ->
        createDirectoryW txPtr nullPtr
    when (0 == errCreateDir) $ do throwLastError "junction:CreateDirectoryW"
    errCreateJuncDir <- withLPCWSTR junctionDir $ \txPtr ->
        createDirectoryW txPtr nullPtr
    when (0 == errCreateJuncDir) $ do throwLastError "junction1:CreateDirectoryW"

    -- get target dir handle
    junctionHandle <- withLPCWSTR junctionDir $ \txPtr -> do
        let accessFlags = 0x40000000 -- GENERIC_WRITE
        let disposition = 3 -- OPEN_EXISTING
        -- FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS
        let flags = 0x00200000 .|. 0x02000000
        createFileW txPtr accessFlags 0 nullPtr disposition flags nullPtr
    when (-1 == ptrToIntPtr junctionHandle) $ do throwLastError "junction:CreateFileW"

    -- create junction
    cwd <- getCwd
    let target = "\\??\\" <> cwd <> "\\" <> junctionTarget <> "\\"
    let targetLen = lengthWord16 target
    let targetLenBytes = targetLen * 2
    vec <- replicate targetLen (0 :: Word16)
    _ <- unsafeWith vec $ \ptr -> unsafeCopyToPtr target ptr
    vecFrozen <- freeze vec
    let bufVal = ReparsePointBuf
            { reparseTag = 0xa0000003 -- IO_REPARSE_TAG_MOUNT_POINT
            , reparseDataLength = fromIntegral (targetLenBytes + 10)
            , reserved = 0
            , reparseTargetLength = fromIntegral (targetLenBytes - 2)
            , reparseTargetMaximumLength = fromIntegral targetLenBytes
            , reserved1 = 0
            , reparseTarget = vecFrozen
            }
    bufVec <- replicate (sizeOf bufVal) (0 :: Word8) 
    errDIO <- unsafeWith bufVec $ \ptr -> do
        _ <- poke (castPtr ptr) bufVal
        bufRet <- replicate 1 (0 :: CUInt)
        unsafeWith bufRet $ \ret -> do
            let ha = junctionHandle
            let cc = 0x000900a4 -- FSCTL_SET_REPARSE_POINT
            let src = castPtr ptr
            let size = fromIntegral (targetLenBytes + 10 + 8) -- + REPARSE_MOUNTPOINT_HEADER_SIZE
            let dest = castPtr ret
            deviceIoControl ha cc src size nullPtr 0 dest nullPtr
    when (0 == errDIO) $ do throwLastError "junction:DeviceIoControl"

    -- check junction
    let fooFileTarget = junctionTarget <> "\\foo.txt"
    let fooFileJunction = junctionDir <> "\\foo.txt"
    _ <- writeFile (unpack fooFileTarget) "foo"
    readThroughJunction <- readFile (unpack fooFileJunction)
    when ("foo" /= readThroughJunction) $ error "Junction read fail"
    errDelFile <- withLPCWSTR fooFileJunction $ \txPtr ->
        deleteFileW txPtr
    when (0 == errDelFile) $ do throwLastError "junction:DeleteFileW"

    -- remove junction
    headerVec <- replicate 8 (0 :: Word8)
    errDIORem <- unsafeWith headerVec $ \buf -> do
        _ <- pokeByteOff buf 0 (0xa0000003 :: CUInt) 
        bufRet <- replicate 1 (0 :: CUInt)
        unsafeWith bufRet $ \ret -> do
            let ha = junctionHandle
            let cc = 0x000900ac -- FSCTL_DELETE_REPARSE_POINT
            let src = castPtr buf
            let size = 8 -- REPARSE_MOUNTPOINT_HEADER_SIZE
            let dest = castPtr ret
            deviceIoControl ha cc src size nullPtr 0 dest nullPtr
    when (0 == errDIORem) $ do throwLastError "junction1:DeviceIoControl"

    -- close target dir
    errCloseHa <- closeHandle junctionHandle
    when (0 == errCloseHa) $ do throwLastError "junction:CloseHandle"

    -- cleanup
    errRemoveDir <- withLPCWSTR junctionDir $ \txPtr ->
        removeDirectoryW txPtr
    when (0 == errRemoveDir) $ do throwLastError "junction:RemoveDirectoryW"
    errRemoveDirJunc <- withLPCWSTR junctionTarget $ \txPtr ->
        removeDirectoryW txPtr
    when (0 == errRemoveDirJunc) $ do throwLastError "junction1:RemoveDirectoryW"
    return ()

symlinkSample :: IO ()
symlinkSample = do
    putStrLn "Running symlink sample ..."
    let symlinkTarget = workDir <> "\\symlink_target"
    let symlinkTargetRel = "symlink_target"
    let symlinkEntry = workDir <> "\\symlink_entry"

    -- write target file
    _ <- writeFile (unpack symlinkTarget) "foo"

    -- create symlink
    errSymlink <- withLPCWSTR symlinkTargetRel $ \targetPtr ->
        withLPCWSTR symlinkEntry $ \entryPtr ->
            createSymbolicLinkW entryPtr targetPtr 0
    when ((0 :: CUChar) == errSymlink) $ do throwLastError "symlink:CreateSymbolicLinkW"

    -- check link
    readThroughLink <- readFile (unpack symlinkEntry)
    when ("foo" /= readThroughLink) $ error "Symlink read fail"

    -- cleanup
    errDelLink <- withLPCWSTR symlinkEntry $ \ptr ->
        deleteFileW ptr
    when (0 == errDelLink) $ do throwLastError "symlink:DeleteFileW"
    errDelFile <- withLPCWSTR symlinkTarget $ \ptr ->
        deleteFileW ptr
    when (0 == errDelFile) $ do throwLastError "symlink1:DeleteFileW"

    return ()

main :: IO ()
main = do
    putStrLn "Creating work dir ..."
    errCreateDir <- withLPCWSTR workDir $ \txPtr ->
        createDirectoryW txPtr nullPtr
    when (1 /= errCreateDir) $ do throwLastError "work:CreateDirectoryW" 

    -- junction
    _ <- junctionSample
    -- symlink
    _ <- symlinkSample

    --putStrLn "Performing cleanup ..."
    errRemoveDir <- withLPCWSTR workDir $ \txPtr ->
        removeDirectoryW txPtr
    when (0 == errRemoveDir) $ do throwLastError "work:RemoveDirectoryW" 

    -- finish
    putStrLn "FINISH_SUCCESS"
    return ()

