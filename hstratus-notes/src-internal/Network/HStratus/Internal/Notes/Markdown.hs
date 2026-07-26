{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Phase 1: split a 'NoteText' into paragraphs ('splitIntoParagraphs').
-- Phase 2: render paragraphs as Markdown text ('noteToMarkdown').
module Network.HStratus.Internal.Notes.Markdown
  ( RawParagraph (..)
  , RawSegment (..)
  , noteToMarkdown
  , splitIntoParagraphs
  )
where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Network.HStratus.Internal.Notes.Note
  ( NoteRun (..)
  , NoteStyle (..)
  , NoteText (..)
  )


{- | A paragraph extracted from a 'NoteText', with its resolved style and
ordered inline segments.
-}
data RawParagraph = RawParagraph
  { rpStyle :: Maybe NoteStyle
  -- ^ Style of the paragraph; 'Nothing' for plain body text.
  , rpSegments :: [RawSegment]
  -- ^ Inline segments in order; may be empty for blank paragraphs.
  }
  deriving (Eq, Show)


-- | One inline span within a 'RawParagraph'.
data RawSegment = RawSegment
  { rsText :: Text
  , rsBold :: Bool
  , rsItalic :: Bool
  , rsStrikethrough :: Bool
  , rsUnderline :: Bool
  , rsLink :: Maybe Text
  }
  deriving (Eq, Show)


-- Internal fold state.
data SplitState = SplitState
  { ssRemaining :: Text
  , ssCurrentStyle :: Maybe NoteStyle
  , ssCurrentSegs :: [RawSegment] -- reversed; reversed on paragraph close
  , ssDone :: [RawParagraph] -- reversed; reversed in finalize
  }


{- | Split a 'NoteText' into paragraphs.

Each @\\n@ in the note text closes the current paragraph and opens a new one.
Runs with @nrStyle = Nothing@ (neutral/inline-only) are absorbed into the
current paragraph rather than starting a new one; the paragraph's style is
taken from the first run in that paragraph that carries a non-'Nothing'
'nrStyle'.

@\\xFFFC@ (Unicode object replacement character) in each run's text is
replaced with @[attachment: \<id\>]@ when 'nrAttachmentId' is present, or
@[attachment]@ otherwise.
-}
splitIntoParagraphs :: NoteText -> [RawParagraph]
splitIntoParagraphs NoteText{ntText, ntRuns} =
  let finalState = foldl' processRun initialState ntRuns
   in finalize finalState
 where
  initialState =
    SplitState
      { ssRemaining = ntText
      , ssCurrentStyle = Nothing
      , ssCurrentSegs = []
      , ssDone = []
      }

  processRun :: SplitState -> NoteRun -> SplitState
  processRun st run =
    let n = max 0 (fromIntegral (nrLength run))
        (slice, remaining') = T.splitAt n (ssRemaining st)
        slice' = replaceAttachment (nrAttachmentId run) slice
        parts = T.splitOn "\n" slice'
        newStyle = maybe (nrStyle run) Just (ssCurrentStyle st)
        mkSeg txt =
          RawSegment
            { rsText = txt
            , rsBold = nrBold run
            , rsItalic = nrItalic run
            , rsStrikethrough = nrStrikethrough run
            , rsUnderline = nrUnderline run
            , rsLink = nrLink run
            }
        addSeg txt segs = if T.null txt then segs else mkSeg txt : segs
        closePara style segs =
          RawParagraph{rpStyle = style, rpSegments = reverse segs}
     in case parts of
          [] ->
            st{ssRemaining = remaining', ssCurrentStyle = newStyle}
          [single] ->
            st
              { ssRemaining = remaining'
              , ssCurrentStyle = newStyle
              , ssCurrentSegs = addSeg single (ssCurrentSegs st)
              }
          (firstPart : moreParts) ->
            let segsWithFirst = addSeg firstPart (ssCurrentSegs st)
                closedFirst = closePara newStyle segsWithFirst
                (finalDone, finalSegs) =
                  foldPartsAfterFirst mkSeg moreParts (closedFirst : ssDone st)
             in st
                  { ssRemaining = remaining'
                  , ssCurrentStyle = Nothing
                  , ssCurrentSegs = finalSegs
                  , ssDone = finalDone
                  }

  finalize :: SplitState -> [RawParagraph]
  finalize st =
    let lastPara =
          RawParagraph
            { rpStyle = ssCurrentStyle st
            , rpSegments = reverse (ssCurrentSegs st)
            }
     in reverse (lastPara : ssDone st)


-- After the first '\n' in a run, fold over the remaining parts: all but the
-- last are closed as single-segment paragraphs; the last stays open.
foldPartsAfterFirst
  :: (Text -> RawSegment)
  -> [Text]
  -> [RawParagraph]
  -> ([RawParagraph], [RawSegment])
foldPartsAfterFirst _ [] done = (done, [])
foldPartsAfterFirst mk [p] done =
  (done, if T.null p then [] else [mk p])
foldPartsAfterFirst mk (p : ps) done =
  let segs = if T.null p then [] else [mk p]
      para = RawParagraph{rpStyle = Nothing, rpSegments = segs}
   in foldPartsAfterFirst mk ps (para : done)


replaceAttachment :: Maybe Text -> Text -> Text
replaceAttachment mId = T.replace "\xFFFC" placeholder
 where
  placeholder =
    maybe "[attachment]" (\i -> "[attachment: " <> i <> "]") mId


{- | Render a 'NoteText' as Markdown.

Paragraphs are separated by double newlines.  Empty paragraphs are dropped.
Supported paragraph styles:

* 'StyleTitle'       → @# …@
* 'StyleHeading'     → @## …@
* 'StyleSubheading'  → @### …@
* 'StyleBody True'   → @> …@ (block-quote)

All other styles (including list and monospaced styles) render as plain body
text; dedicated rendering for those styles is deferred to future steps.

Inline formatting: bold (@**@), italic (@_@), strikethrough (@~~@),
link (@[text](url)@).  Underline has no Markdown equivalent and is dropped.
-}
noteToMarkdown :: NoteText -> Text
noteToMarkdown nt =
  T.intercalate "\n\n" (map renderParagraph (filter hasContent (splitIntoParagraphs nt)))


hasContent :: RawParagraph -> Bool
hasContent = any (not . T.null . rsText) . rpSegments


renderParagraph :: RawParagraph -> Text
renderParagraph RawParagraph{rpStyle, rpSegments} =
  paragraphPrefix rpStyle <> T.concat (map renderSegment rpSegments)


paragraphPrefix :: Maybe NoteStyle -> Text
paragraphPrefix (Just StyleTitle) = "# "
paragraphPrefix (Just StyleHeading) = "## "
paragraphPrefix (Just StyleSubheading) = "### "
paragraphPrefix (Just (StyleBody True)) = "> "
paragraphPrefix _ = ""


renderSegment :: RawSegment -> Text
renderSegment RawSegment{rsText, rsBold, rsItalic, rsStrikethrough, rsLink} =
  let inner = applyBoldItalic rsBold rsItalic rsText
      withStrike = if rsStrikethrough then "~~" <> inner <> "~~" else inner
   in case rsLink of
        Nothing -> withStrike
        Just url -> "[" <> withStrike <> "](" <> url <> ")"


applyBoldItalic :: Bool -> Bool -> Text -> Text
applyBoldItalic True True t = "**_" <> t <> "_**"
applyBoldItalic True False t = "**" <> t <> "**"
applyBoldItalic False True t = "_" <> t <> "_"
applyBoldItalic False False t = t
