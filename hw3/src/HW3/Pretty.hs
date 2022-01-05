module HW3.Pretty (
    prettyValue
    ) where

import HW3.Base (HiValue (..))
import Prettyprinter ( Doc, Pretty (pretty) )
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber val) = pretty (show val)
prettyValue (HiValueBool val) = pretty (show val)
prettyValue (HiValueFunction val) = pretty (show val)