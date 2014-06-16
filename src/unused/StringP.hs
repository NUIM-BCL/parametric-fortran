-- StringP.hs

module StringP where

import Param
import Data.Generics
import FortranP

instance Param String Program  where
instance Param String Arg      where
instance Param String ArgList  where
instance Param String Fortran  where
instance Param String Expr     where
instance Param String Type     where
instance Param String Decl     where
instance Param String Block    where
instance Param String SubName  where
instance Parma String BaseType where

-- You may need add more instance definitions for
-- other Fortran syntax categories.
