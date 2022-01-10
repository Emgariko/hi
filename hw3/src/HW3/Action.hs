module HW3.Action (
        HiPermission(..),
        HIO(..)
    ) where
import Control.Exception (Exception, throwIO)
import Data.Set (Set, notMember)
import HW3.Base (HiMonad (..), HiAction (..), HiValue (..))
import Control.Monad (ap)
import qualified Data.ByteString as ByteString
import Data.ByteString (readFile)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist, createDirectory, getCurrentDirectory, setCurrentDirectory)
import Data.Sequence (fromList)
import qualified Data.Text
import Data.Text.Encoding (decodeUtf8')

data HiPermission = AllowRead
    | AllowWrite
    deriving (Show, Eq, Ord)

data PermissionException =
  PermissionRequired HiPermission
    deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
    fmap f HIO { runHIO = g } = HIO { runHIO = fmap f . g}
    -- fmap = liftM

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

instance HiMonad HIO where
    runAction (HiActionRead path) = HIO { runHIO =
        \perms ->
            if notMember AllowRead perms
            then throwIO $ PermissionRequired AllowRead
            else do
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
                            Left _ -> HiValueBytes fileContent
                            Right text -> HiValueString text
                    else 
                        return HiValueNull
    }
    runAction (HiActionWrite path bts) = HIO { runHIO =
        \perms ->
            if notMember AllowWrite perms
            then throwIO $ PermissionRequired AllowWrite
            else do
                ByteString.writeFile path bts
                return HiValueNull
    }
    runAction (HiActionMkDir path) = HIO { runHIO = 
        \perms -> 
            if notMember AllowWrite perms
            then throwIO $ PermissionRequired AllowWrite
            else do
                _ <- createDirectory path
                return HiValueNull
    }
    runAction (HiActionChDir path) = HIO { runHIO = 
        \perms ->
            if notMember AllowRead perms
            then throwIO $ PermissionRequired AllowRead
            else do
                _ <- setCurrentDirectory path
                return HiValueNull
    }
    runAction HiActionCwd = HIO { runHIO = 
        \perms ->
            if notMember AllowRead perms
            then throwIO $ PermissionRequired AllowRead
            else do
                dir <- getCurrentDirectory
                return $ HiValueString . Data.Text.pack $ dir
    }
