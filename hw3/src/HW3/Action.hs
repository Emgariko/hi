module HW3.Action (
        HiPermission(..),
        HIO(..),
        PermissionException (..)
    ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (ap)
import Data.ByteString (readFile)
import qualified Data.ByteString as ByteString
import Data.Sequence (fromList)
import Data.Set (Set, notMember)
import qualified Data.Text
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.Random.Stateful (getStdRandom, uniformR)

data HiPermission = AllowRead
    | AllowWrite
    | AllowTime
    deriving (Show, Eq, Ord)

data PermissionException =
  PermissionRequired HiPermission
    deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
    fmap f HIO { runHIO = g } = HIO { runHIO = fmap f . g}

instance Applicative HIO where
    pure = return
    (<*>) = ap

instance Monad HIO where
    return x = HIO $ \_ -> return x
    m >>= f =
        (\m -> HIO {    -- join
        runHIO = \x -> do
            inner <- runHIO m x
            runHIO inner x
        }) (fmap f m)

checkPermissionAndThenDo :: Maybe HiPermission -> IO a -> Set HiPermission -> IO a
checkPermissionAndThenDo maybePerm d perms =
    case maybePerm of
        Nothing -> d
        (Just requiredPerm) -> if notMember requiredPerm perms
                        then throwIO $ PermissionRequired requiredPerm
                        else d

instance HiMonad HIO where
    runAction (HiActionRead path) = HIO { runHIO =
        checkPermissionAndThenDo (Just AllowRead) (do
                dirExists <- doesDirectoryExist path
                fileExists <- doesFileExist path
                if dirExists
                then do
                    dirContent <- listDirectory path
                    return $ HiValueList $ fromList $ map (HiValueString . Data.Text.pack) dirContent
                else if fileExists
                    then do
                        fileContent <- ByteString.readFile path
                        return $ case decodeUtf8' fileContent of
                            Left _     -> HiValueBytes fileContent
                            Right text -> HiValueString text
                    else
                        return HiValueNull
        )
    }
    runAction (HiActionWrite path bts) = HIO { runHIO =
        checkPermissionAndThenDo (Just AllowWrite) (do
            ByteString.writeFile path bts
            return HiValueNull
        )
    }
    runAction (HiActionMkDir path) = HIO { runHIO =
        checkPermissionAndThenDo (Just AllowWrite) (do
            _ <- createDirectory path
            return HiValueNull
        )
    }
    runAction (HiActionChDir path) = HIO { runHIO =
        checkPermissionAndThenDo (Just AllowRead) (do
            _ <- setCurrentDirectory path
            return HiValueNull)
    }
    runAction HiActionCwd = HIO { runHIO =
        checkPermissionAndThenDo (Just AllowRead) (do
            dir <- getCurrentDirectory
            return $ HiValueString . Data.Text.pack $ dir
        )
    }
    runAction HiActionNow = HIO { runHIO =
        checkPermissionAndThenDo (Just AllowTime) (
            HiValueTime <$> getCurrentTime
        )
    }
    runAction (HiActionRand l r) = HIO { runHIO =
        checkPermissionAndThenDo Nothing (do
            v <- getStdRandom (uniformR (l, r))
            return $ (HiValueNumber . toRational) v
        )
    }
    runAction (HiActionEcho text) = HIO { runHIO =
        checkPermissionAndThenDo (Just AllowWrite) (do
            putStrLn $ Data.Text.unpack text
            return HiValueNull
        )
    }
