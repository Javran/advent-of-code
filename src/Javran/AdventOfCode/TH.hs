module Javran.AdventOfCode.TH
  ( collectSolutions
  , module Data.Proxy
  )
where

import Data.Maybe
import Data.Proxy
import Language.Haskell.TH

collectSolutions :: Q Exp
collectSolutions = do
  (Just subcmdTypeName) <- lookupTypeName "Solution"
  ClassI _ ins <- reify subcmdTypeName
  (Just proxyTypeName) <- lookupTypeName "Proxy"
  (Just proxyValName) <- lookupValueName "Proxy"
  (Just someSolValName) <- lookupValueName "SomeSolution"
  let typNames = mapMaybe getTypes ins
      gen :: Name -> Exp
      gen n = AppE (ConE someSolValName) inner
        where
          innerV = ConE proxyValName
          innerT = AppT (ConT proxyTypeName) (ConT n)
          inner = innerV `SigE` innerT
  pure (ListE (gen <$> typNames))
  where
    getTypes :: InstanceDec -> Maybe Name
    getTypes (InstanceD _ _ (AppT _ (ConT tyN)) _) = Just tyN
    getTypes _ = Nothing
