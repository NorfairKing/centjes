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
  { sourceSpanFile :: !FilePath,
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

combineSpans :: SourceSpan -> SourceSpan -> SourceSpan
combineSpans begin end = begin {sourceSpanEnd = sourceSpanEnd end}

data SourcePosition = SourcePosition
  { -- Not that these should be 'Word's, but I'd rather not use 'fromIntegral'
    -- to go back and forth from Happy to this to Diagnose which both use 'Int's.
    sourcePositionLine :: !Int,
    sourcePositionColumn :: !Int
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity SourcePosition

instance NFData SourcePosition
