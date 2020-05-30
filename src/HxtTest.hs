{-# LANGUAGE Arrows #-}

module HxtTest(test) where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
-- import Data.Tree.Class
import System.Exit
import Debug.Trace


-- type XmlTrees = NTrees XNode
-- type NTrees a = [NTree a]
-- data XNode      = XText           String                        -- ^ ordinary text                                       (leaf)
--                 | XBlob           Blob                          -- ^ text represented more space efficient as bytestring (leaf)
--                 | XCharRef        Int                           -- ^ character reference                                 (leaf)
--                 | XEntityRef      String                        -- ^ entity reference                                    (leaf)
--                 | XCmt            String                        -- ^ comment                                             (leaf)
--                 | XCdata          String                        -- ^ CDATA section                                       (leaf)
--                 | XPi             QName XmlTrees                -- ^ Processing Instr with qualified name                (leaf)
--                                                                 --   with list of attributes.
--                                                                 --   If tag name is xml, attributes are \"version\", \"encoding\", \"standalone\",
--                                                                 --   else attribute list is empty, content is a text child node
--                 | XTag            QName XmlTrees                -- ^ tag with qualified name and list of attributes (inner node or leaf)
--                 | XDTD            DTDElem  Attributes           -- ^ DTD element with assoc list for dtd element features
--                 | XAttr           QName                         -- ^ attribute with qualified name, the attribute value is stored in children
--                 | XError          Int  String                   -- ^ error message with level and text
--                   deriving (Eq, Show, Typeable)


inFile :: FilePath
inFile = "./Example/Simple.xml"

outFile :: FilePath
outFile = "./Example/out.xml"

test :: IO()
test = do
    [rc]  <- runX (application inFile outFile)
    if rc >= c_err
        then exitWith $ ExitFailure 1 
        else exitSuccess

application :: String -> String -> IOSArrow b Int
application src dst = readDocument [] src >>> processChildren (processDocumentRootElement `when` isElem) >>> writeDocument [] dst >>> getErrStatus

processDocumentRootElement :: IOSArrow XmlTree XmlTree
processDocumentRootElement = proc iXml -> do
    oXml <- changeChildren filterChildren -< iXml
    returnA -< oXml

filterChildren :: [NTree XNode] -> [NTree XNode]
filterChildren x = traceShow tagNodes tagNodes
    where
        tagNodes = filter isTag x

isTag :: NTree XNode -> Bool
isTag (NTree (XTag _ _) _) = True
isTag _ = False
