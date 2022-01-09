module HW3.Action (
        HiPermission(..)
    ) where 
import Control.Exception (Exception)
import Data.Set (Set)

data HiPermission = AllowRead
    | AllowWrite 
    deriving Show

data PermissionException =
  PermissionRequired HiPermission 
    deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }