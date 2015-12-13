module HintExample where

import SortWrapper (Sort)
import qualified Language.Haskell.Interpreter as I
import Language.Haskell.Interpreter (OptionVal((:=)))

-- | Dynamically load a 'Sort' implementation from a file.
-- src is needed to pick up our SortWrapper.
-- sort-plugins is a sample user plugins directory
loadSort :: I.MonadInterpreter m =>
            String  -- ^ module name
         -> String  -- ^ function name
         -> m Sort
loadSort moduleName functionName = do
  I.set [I.searchPath := ["src", "sort-plugins"]]
  I.loadModules [moduleName]
  I.setImports [moduleName, "SortWrapper"]
  I.interpret (moduleName ++ "." ++ functionName) (I.as :: Sort)
