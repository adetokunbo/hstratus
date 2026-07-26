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

import qualified Data.IntMap.Strict as IM
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


-- Internal state for the markdown renderer.
data RenderState = RenderState
  { rsCounters :: IM.IntMap Int
  -- ^ Counter per indent level for numbered lists.
  , rsPrevStyle :: Maybe NoteStyle
  -- ^ Style of the previous paragraph, for numbered-list group-start detection.
  }


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

Consecutive list paragraphs are separated by a single newline; all other
paragraph boundaries use a double newline.  Empty paragraphs are dropped.
Supported paragraph styles:

* 'StyleTitle'         → @# …@
* 'StyleHeading'       → @## …@
* 'StyleSubheading'    → @### …@
* 'StyleBody True'     → @> …@ (block-quote)
* 'StyleBullet i'      → @- …@ (indented by @i × 2@ spaces)
* 'StyleDash i'        → @- …@ (indented by @i × 2@ spaces)
* 'StyleNumbered i ms' → @N. …@ (auto-counter per indent level)
* 'StyleChecklist i b' → @- [x] …@ or @- [ ] …@

'StyleMonospaced' is deferred; it renders as plain body text for now.

Inline formatting: bold (@**@), italic (@_@), strikethrough (@~~@),
link (@[text](url)@).  Underline has no Markdown equivalent and is dropped.
-}
noteToMarkdown :: NoteText -> Text
noteToMarkdown nt =
  T.concat (go (RenderState{rsCounters = IM.empty, rsPrevStyle = Nothing}) (filter hasContent (splitIntoParagraphs nt)))
 where
  go _ [] = []
  go st [p] =
    let (_, rendered) = renderParagraphWith st p
     in [rendered]
  go st (p : rest@(next : _)) =
    let (st', rendered) = renderParagraphWith st p
        sep = if isListPara p && isListPara next then "\n" else "\n\n"
     in rendered : sep : go st' rest


isListPara :: RawParagraph -> Bool
isListPara RawParagraph{rpStyle} = case rpStyle of
  Just (StyleBullet _) -> True
  Just (StyleDash _) -> True
  Just (StyleNumbered _ _) -> True
  Just (StyleChecklist _ _) -> True
  _ -> False


hasContent :: RawParagraph -> Bool
hasContent = any (not . T.null . rsText) . rpSegments


renderParagraphWith :: RenderState -> RawParagraph -> (RenderState, Text)
renderParagraphWith st RawParagraph{rpStyle, rpSegments} =
  let (st', prefix) = resolvePrefix st rpStyle
      content = T.concat (map renderSegment rpSegments)
   in (st', prefix <> content)


resolvePrefix :: RenderState -> Maybe NoteStyle -> (RenderState, Text)
resolvePrefix st style =
  let st' = st{rsPrevStyle = style}
   in case style of
        Just (StyleBullet i) -> (st', indentText i <> "- ")
        Just (StyleDash i) -> (st', indentText i <> "- ")
        Just (StyleChecklist i b) ->
          (st', indentText i <> if b then "- [x] " else "- [ ] ")
        Just (StyleNumbered i ms) ->
          let (n, counters') = nextCounter (rsCounters st) i ms (rsPrevStyle st)
           in (st'{rsCounters = counters'}, indentText i <> T.pack (show n) <> ". ")
        _ -> (st', staticPrefix style)


staticPrefix :: Maybe NoteStyle -> Text
staticPrefix (Just StyleTitle) = "# "
staticPrefix (Just StyleHeading) = "## "
staticPrefix (Just StyleSubheading) = "### "
staticPrefix (Just (StyleBody True)) = "> "
staticPrefix _ = ""


{- | Returns the counter value to emit and the updated counter map.
Group-start (first item or resume after non-numbered paragraph):
resets to @ms@ (or 1 if absent).  Continuation: uses the running counter,
ignoring @ms@ even if Apple Notes emits it on every item.
-}
nextCounter :: IM.IntMap Int -> Int -> Maybe Int -> Maybe NoteStyle -> (Int, IM.IntMap Int)
nextCounter counters i ms prevStyle =
  let isGroupStart = case prevStyle of
        Just (StyleNumbered j _) -> j /= i
        _ -> True
      startVal
        | isGroupStart = maybe 1 id ms
        | otherwise = IM.findWithDefault 1 i counters
   in (startVal, IM.insert i (startVal + 1) counters)


indentText :: Int -> Text
indentText i = T.replicate (max 0 i * 2) " "


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
