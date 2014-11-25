-- This program takes a podcast feed XML file and outputs a modified
-- version with a new item prepended and the oldest item removed.  The
-- contents of the new item are specified as arguments.

import Text.XML.HXT.Core

import System.Environment
import System.IO
import System.Exit

-- make a new item using command-line arguments for the contents:
makeItem argv = selem "item"
  [ selem "title" [ txt (argv!!0) ]
  , selem "link" [ txt (argv!!1) ]
  , eelem "description"
  , aelem "enclosure" [ sattr "url" (argv!!2)
                      , sattr "length" (argv!!3)
                      , sattr "type" "audio/mpeg" ]
  , selem "itunes:duration" [ txt (argv!!4) ]
  , selem "itunes:author" [ txt (argv!!5) ]
  , selem "pubDate" [ txt (argv!!6) ]
  , selem "dc:creator" [ txt "admin" ]
  , mkelem "guid" [ sattr "isPermaLink" "false" ] [ txt (argv!!7) ] ]

-- place all non-item elements at the top of the child list and
-- prepend a new item
processChannel argv = replaceChildren
  ((getChildren >>> isElem >>> (neg (hasName "item")))
   <+> (makeItem argv)
   <+> (getChildren >>> isElem >>> hasName "item"))

-- find and transform any channel elements found, leaving everything
-- else unchanged:
processRoot argv = processTopDown
  ((processChannel argv) `when` (isElem >>> hasName "channel"))

-- read the feed from standard input and write a modified version to
-- standard output:
main = do
  argv <- getArgs
  if length argv /= 8
    then do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name
        ++ " <title> <feed link> <mp3 link> <mp3 length in bytes>"
        ++ " <duration MM:SS> <author> <publish date> <guid>"
      exitFailure
    else do
  runX ( readDocument [ withValidate no ] "stdin:"
         >>> processChildren ((processRoot argv) `when` isElem)
         >>> writeDocument [ withIndent yes
                           , withOutputEncoding Text.XML.HXT.Core.utf8 ] "-"
       )
  return ()
