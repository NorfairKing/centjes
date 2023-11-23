{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Location where

import Control.DeepSeq
import Data.Validity
import qualified Error.Diagnose.Position as Diagnose (Position (..))
import GHC.Generics (Generic)

data GenLocated l e = Located
  { locatedLocation :: !l,
    locatedValue :: !e
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Functor (GenLocated l) where
  fmap f (Located l v) = Located l (f v)

instance Foldable (GenLocated l) where
  foldMap f = f . locatedValue

instance Traversable (GenLocated l) where
  traverse f (Located s v) = Located s <$> f v

instance (Validity l, Validity a) => Validity (GenLocated l a)

instance (NFData l, NFData a) => NFData (GenLocated l a)

type Located a = GenLocated SourceSpan a

type LLocated a = GenLocated SourceSpan (a SourceSpan)

noLoc :: a -> GenLocated () a
noLoc = Located ()

data SourceSpan = SourceSpan
  { sourceSpanFile :: !FilePath, -- TODO put an absolute filepath here
    sourceSpanBegin :: !SourcePosition, -- Should be words, but I'd rather not use 'fromIntegral'.
    sourceSpanEnd :: !SourcePosition
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity SourceSpan

instance NFData SourceSpan

toDiagnosePosition :: SourceSpan -> Diagnose.Position
toDiagnosePosition SourceSpan {..} =
  Diagnose.Position
    { begin = (sourcePositionLine sourceSpanBegin, sourcePositionColumn sourceSpanBegin),
      end = (sourcePositionLine sourceSpanEnd, sourcePositionColumn sourceSpanEnd),
      file = sourceSpanFile
    }

-- TODO maybe refactor this out into a module
data SourcePosition = SourcePosition
  { sourcePositionLine :: !Int, -- Should be words, but I'd rather not use 'fromIntegral'.
    sourcePositionColumn :: !Int
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity SourcePosition

instance NFData SourcePosition
