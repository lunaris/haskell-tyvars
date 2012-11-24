{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TyVars where

import Control.Monad.State
import Data.List
import Language.Haskell.Exts

import qualified Data.Label as L
import qualified Data.Label.PureM as P
import qualified Data.Map as M

extendedParseMode :: ParseMode
extendedParseMode
  = ParseMode { parseFilename         = ""
              , extensions            = mostExtensions
              , ignoreLinePragmas     = False
              , ignoreLanguagePragmas = False
              , fixities              = Just baseFixities
              }

mostExtensions :: [Extension]
mostExtensions
  = [ Arrows
    , BangPatterns
    , FlexibleContexts
    , FlexibleInstances
    , ForeignFunctionInterface
    , FunctionalDependencies
    , GADTs
    , GeneralizedNewtypeDeriving
    , KindSignatures
    , LiberalTypeSynonyms
    , MagicHash
    , MultiParamTypeClasses
    , QuasiQuotes
    , RankNTypes
    , ScopedTypeVariables
    , TemplateHaskell
    , TupleSections
    , TypeFamilies
    , TypeOperators
    , TypeSynonymInstances
    , UnboxedTuples
    ]

parseExtendedType :: String -> Either String Type
parseExtendedType s
  = case parseWithMode extendedParseMode s of
      ParseOk t           -> Right t
      ParseFailed loc err -> Left err

splitType :: Type -> [Type]
splitType (TyForall _ _ t)
  = splitType t

splitType (TyFun t1 t2)
  = t1 : splitType t2

splitType t
  = [t]

data VState
  = VState  { _vsMap        :: M.Map String Int
            , _vsScalars    :: [String]
            , _vsFunctions  :: [String]
            }

L.mkLabels [''VState]

emptyVState :: VState
emptyVState
  = VState  { _vsMap        = M.empty
            , _vsScalars    = map (:[]) "xyzwvumnijksteabcpqr"
            , _vsFunctions  = map (:[]) "fghk"
            }

newtype V a
  = V { runV :: State VState a }
  deriving (Functor, Monad, MonadState VState)

evalV :: V a -> a
evalV
  = flip evalState emptyVState . runV

fresh :: String -> V String
fresh base
  = V $ do
      m <- P.gets vsMap

      let f _ _ x       = x + 1
          (maybeN, m')  = M.insertLookupWithKey f base 1 m
          v             = base ++ maybe "" show maybeN

      P.puts vsMap m'
      return v

freshPreset :: (VState L.:-> [String]) -> V String
freshPreset f
  = V $ do
      s : ss <- P.gets f
      P.puts f (ss ++ [s])
      runV (fresh s)

freshScalar :: V String
freshScalar
  = freshPreset vsScalars

freshFunction :: V String
freshFunction
  = freshPreset vsFunctions

tyVar :: Type -> V String
tyVar (TyApp t1 t2)
  | s1 == "m"     = fresh "m"
  | s1 == "Maybe" = fmap ('m' :) (tyVar t2)
  where
    s1 = prettyPrint t1

tyVar (TyForall _ _ t)
  = tyVar t

tyVar (TyFun _ _)
  = freshFunction

tyVar (TyParen t)
  = tyVar t

tyVar (TyList t)
  = fmap (++ "s") (tyVar t)

tyVar t
  | s == "Char"           = fresh "c"
  | "Exp" `isPrefixOf` s  = fresh "e"
  | s == "String"         = fresh "s"
  | s == "World"          = fresh "w"
  | otherwise             = freshScalar
  where
    s = prettyPrint t

main :: IO ()
main
  = interact $
      either (const "") (unwords . evalV . mapM tyVar . init . splitType) .
        parseExtendedType
