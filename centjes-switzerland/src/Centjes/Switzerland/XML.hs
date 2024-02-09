{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.XML where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.DecimalLiteral (DecimalLiteral)
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Text.XML as XML

class ToElement a where
  toElement :: a -> XML.Element

class ToNodes a where
  toNodes :: a -> [XML.Node]

ech0058Element :: Text -> [XML.Node] -> XML.Element
ech0058Element name = xmlElement (ech0058Name name)

ech0058Name :: Text -> XML.Name
ech0058Name = xmlName ech0058Url ech0058Abbreviation

ech0058Abbreviation :: Text
ech0058Abbreviation = "eCH-0058"

ech0058Url :: Text
ech0058Url = "http://www.ech.ch/xmlns/eCH-0058/5"

ech0097Element :: Text -> [XML.Node] -> XML.Element
ech0097Element name = xmlElement (ech0097Name name)

ech0097Name :: Text -> XML.Name
ech0097Name = xmlName ech0097Url ech0097Abbreviation

ech0097Abbreviation :: Text
ech0097Abbreviation = "eCH-0097"

ech0097Url :: Text
ech0097Url = "http://www.ech.ch/xmlns/eCH-0097/3"

ech0119Element :: Text -> [XML.Node] -> XML.Element
ech0119Element name = xmlElement (ech0119Name name)

ech0119Name :: Text -> XML.Name
ech0119Name = xmlName ech0119Url ech0119Abbreviation

ech0119Url :: Text
ech0119Url = "http://www.ech.ch/xmlns/eCH-0119/4"

ech0119Abbreviation :: Text
ech0119Abbreviation = "eCH-0119"

ech0217Element :: Text -> [XML.Node] -> XML.Element
ech0217Element name = xmlElement (ech0217Name name)

ech0217Name :: Text -> XML.Name
ech0217Name = xmlName ech0217Url ech0217Abbreviation

ech0217Url :: Text
ech0217Url = "http://www.ech.ch/xmlns/eCH-0217/1"

ech0217Abbreviation :: Text
ech0217Abbreviation = "eCH-0217"

xsiName :: Text -> XML.Name
xsiName = xmlName xsiUrl xsiAbbreviation

xsiAbbreviation :: Text
xsiAbbreviation = "xsi"

xsiUrl :: Text
xsiUrl = "http://www.w3.org/2001/XMLSchema-instance"

xmlElement :: XML.Name -> [XML.Node] -> XML.Element
xmlElement elementName elementNodes =
  let elementAttributes = M.empty
   in XML.Element {..}

decimalLiteralNode :: DecimalLiteral -> XML.Node
decimalLiteralNode =
  XML.NodeContent
    . T.pack
    . DecimalLiteral.format
    . DecimalLiteral.setSignOptional

-- | Render with the four relevant namespaces and an XML declaration.
xmlRenderSettings :: XML.RenderSettings
xmlRenderSettings =
  def
    { rsXMLDeclaration = True,
      rsNamespaces =
        [ (ech0058Abbreviation, ech0058Url),
          (ech0097Abbreviation, ech0097Url),
          (ech0119Abbreviation, ech0119Url),
          (ech0217Abbreviation, ech0217Url),
          -- ("ech0007f", "http://www.ech.ch/xmlns/eCH-0007-f/6"),
          -- ("ech0011f", "http://www.ech.ch/xmlns/eCH-0011-f/8"),
          -- ("ech0044f", "http://www.ech.ch/xmlns/eCH-0044-f/4"),
          -- ("ech0046f", "http://www.ech.ch/xmlns/eCH-0046-f/5"),
          (xsiAbbreviation, xsiUrl)
        ]
    }

xmlName :: Text -> Text -> Text -> XML.Name
xmlName namespace prefix nameLocalName =
  let nameNamespace = Just namespace
      namePrefix = Just prefix
   in XML.Name {..}
