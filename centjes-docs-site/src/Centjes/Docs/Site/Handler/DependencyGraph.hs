module Centjes.Docs.Site.Handler.DependencyGraph
  ( getDependencyGraphR,
  )
where

import Centjes.Docs.Site.DependencyGraph
import Centjes.Docs.Site.Handler.Import

getDependencyGraphR :: Handler TypedContent
getDependencyGraphR = case dependencyGraph of
  Nothing -> notFound
  Just contents -> sendResponse (TypedContent typeSvg (toContent contents))
