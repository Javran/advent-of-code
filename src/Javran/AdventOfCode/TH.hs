{-# LANGUAGE TemplateHaskell #-}

module Javran.AdventOfCode.TH
  ( collectSolutions
  , module Data.Proxy
  )
where

import Data.Maybe
import Data.Proxy
import Javran.AdventOfCode.Infra
import Language.Haskell.TH

collectSolutions :: Q Exp
collectSolutions = do
  (Just subcmdTypeName) <- lookupTypeName "Solution"
  ClassI _ ins <- reify subcmdTypeName
  let typNames = mapMaybe getTypes ins
      getTypes :: InstanceDec -> Maybe Type
      getTypes instDec = do
        (InstanceD _ _ (AppT _ c@(ConT _)) _) <- pure instDec
        pure c
  ListE <$> mapM (\n -> [|SomeSolution (Proxy :: Proxy $(pure n))|]) typNames
