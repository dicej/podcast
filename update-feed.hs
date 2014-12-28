-- This program takes a podcast feed XML file and outputs a modified
-- version with a new item prepended and the oldest item removed.  The
-- contents of the new item are specified as arguments.

import Text.XML.HXT.Core

import System.Environment
import System.IO
import System.Exit
import Data.Time
import Data.List
import Data.Char

import Debug.Trace as DT

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

monthNumber month = case month of
  "January" -> 1
  "February" -> 2
  "March" -> 3
  "April" -> 4
  "May" -> 5
  "June" -> 6
  "July" -> 7
  "August" -> 8
  "September" -> 9
  "October" -> 10
  "November" -> 11
  "December" -> 12
  _ -> error ("unexpected month: \"" ++ month ++ "\"")

takeWhileList predicate s =
  if (not (null s)) && predicate s
  then let [ h, t ] = takeWhileList predicate (tail s) in [ (head s):h, t ]
  else [ [], s ]

split divider s =
  let [ h, t ] = takeWhileList (\ s -> not (isPrefixOf divider s)) s
  in [ h, drop (length divider) t ]

takeNumber s = takeWhileList (\ s -> isDigit (head s)) s

endsWith suffix s = suffix == s || ((not (null s)) && endsWith suffix (tail s))

-- Due to an earlier mistake on my part, some titles will be of the
-- form "June 1st, 2014 - The Title - Joe Speaker" and some will be of
-- the form "June 1 - The Title".  The latter form may be assumed to
-- be from 2014.
parseDate title =
  let [ month, monthTail ] = split " " title
      [ day, dayTail ] = split " " monthTail
      fullDate = endsWith "," day
      [ year, _ ] = if fullDate then split " " dayTail else [ "2014", dayTail ]
      [ dayNumber, _ ] = if fullDate then takeNumber day else [ day, [] ]
  in fromGregorian (read year) (monthNumber month) (read dayNumber)

recent newTitle = \ oldTitle ->
  (diffDays (parseDate newTitle) (parseDate oldTitle)) < 365

-- Place all non-item elements at the top of the child list, prepend a
-- new item, and remove any items more than a year older than the new
-- one, as indicated by the dates embedded in the titles:
processChannel argv = replaceChildren
  ((getChildren >>> isElem >>> (neg (hasName "item")))
   <+> (makeItem argv)
   <+> (getChildren >>> isElem >>> hasName "item"
        >>> ((getChildren >>> hasName "title" >>> getChildren
              >>> hasText (recent (argv!!0))) `guards` this)))

-- Find and transform any channel elements found, leaving everything
-- else unchanged:
processRoot argv = processTopDown
  ((processChannel argv) `when` (isElem >>> hasName "channel"))

-- Read the feed from standard input and write a modified version to
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
