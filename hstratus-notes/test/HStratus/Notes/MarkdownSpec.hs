{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : HStratus.Notes.MarkdownSpec
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Tests for the Markdown renderer in
'Network.HStratus.Internal.Notes.Markdown'.
-}
module HStratus.Notes.MarkdownSpec (spec) where

import Data.Text (Text)
import Network.HStratus.Internal.Notes.Markdown
import Network.HStratus.Internal.Notes.Note
import Test.Hspec


spec :: Spec
spec = do
  describe "noteToMarkdown" $ do
    it "renders StyleTitle as h1" $
      noteToMarkdown
        NoteText
          { ntText = "hello"
          , ntRuns = [baseRun{nrLength = 5, nrStyle = Just StyleTitle}]
          }
        `shouldBe` "# hello"

    it "renders StyleHeading as h2" $
      noteToMarkdown
        NoteText
          { ntText = "section"
          , ntRuns = [baseRun{nrLength = 7, nrStyle = Just StyleHeading}]
          }
        `shouldBe` "## section"

    it "renders StyleSubheading as h3" $
      noteToMarkdown
        NoteText
          { ntText = "sub"
          , ntRuns = [baseRun{nrLength = 3, nrStyle = Just StyleSubheading}]
          }
        `shouldBe` "### sub"

    it "renders plain body text without prefix" $
      noteToMarkdown
        NoteText{ntText = "plain", ntRuns = [baseRun{nrLength = 5}]}
        `shouldBe` "plain"

    it "renders StyleBody True as blockquote" $
      noteToMarkdown
        NoteText
          { ntText = "quoted"
          , ntRuns = [baseRun{nrLength = 6, nrStyle = Just (StyleBody True)}]
          }
        `shouldBe` "> quoted"

    it "renders bold segment with ** markers" $
      noteToMarkdown
        NoteText{ntText = "bold", ntRuns = [baseRun{nrLength = 4, nrBold = True}]}
        `shouldBe` "**bold**"

    it "renders italic segment with _ markers" $
      noteToMarkdown
        NoteText{ntText = "ital", ntRuns = [baseRun{nrLength = 4, nrItalic = True}]}
        `shouldBe` "_ital_"

    it "renders bold+italic segment with **_ markers" $
      noteToMarkdown
        NoteText
          { ntText = "bi"
          , ntRuns = [baseRun{nrLength = 2, nrBold = True, nrItalic = True}]
          }
        `shouldBe` "**_bi_**"

    it "renders strikethrough segment with ~~ markers" $
      noteToMarkdown
        NoteText
          { ntText = "strike"
          , ntRuns = [baseRun{nrLength = 6, nrStrikethrough = True}]
          }
        `shouldBe` "~~strike~~"

    it "renders link segment as [text](url)" $
      noteToMarkdown
        NoteText
          { ntText = "click"
          , ntRuns = [baseRun{nrLength = 5, nrLink = Just "https://example.com"}]
          }
        `shouldBe` "[click](https://example.com)"

    it "drops underline with no markup" $
      noteToMarkdown
        NoteText
          { ntText = "under"
          , ntRuns = [baseRun{nrLength = 5, nrUnderline = True}]
          }
        `shouldBe` "under"

    it "renders bold text inside a link" $
      noteToMarkdown
        NoteText
          { ntText = "bold link"
          , ntRuns =
              [ baseRun
                  { nrLength = 9
                  , nrBold = True
                  , nrLink = Just "https://example.com"
                  }
              ]
          }
        `shouldBe` "[**bold link**](https://example.com)"

    it "filters blank paragraphs and joins with double newline" $
      noteToMarkdown
        NoteText{ntText = "a\n\nb", ntRuns = [baseRun{nrLength = 4}]}
        `shouldBe` "a\n\nb"

    it "joins two plain paragraphs with double newline" $
      noteToMarkdown
        NoteText{ntText = "first\nsecond", ntRuns = [baseRun{nrLength = 12}]}
        `shouldBe` "first\n\nsecond"

  describe "noteToMarkdown monospaced" $ do
    it "renders a single StyleMonospaced paragraph as a fenced code block" $
      noteToMarkdown
        NoteText{ntText = "code", ntRuns = [baseRun{nrLength = 4, nrStyle = Just StyleMonospaced}]}
        `shouldBe` "```\ncode\n```"

    it "merges consecutive StyleMonospaced paragraphs into one fence" $
      noteToMarkdown
        NoteText
          { ntText = "line1\nline2"
          , ntRuns =
              [ baseRun{nrLength = 6, nrStyle = Just StyleMonospaced}
              , baseRun{nrLength = 5, nrStyle = Just StyleMonospaced}
              ]
          }
        `shouldBe` "```\nline1\nline2\n```"

    it "closes the fence before a following body paragraph" $
      noteToMarkdown
        NoteText
          { ntText = "code\nbody"
          , ntRuns =
              [ baseRun{nrLength = 5, nrStyle = Just StyleMonospaced}
              , baseRun{nrLength = 4}
              ]
          }
        `shouldBe` "```\ncode\n```\n\nbody"

    it "opens the fence after a preceding body paragraph" $
      noteToMarkdown
        NoteText
          { ntText = "body\ncode"
          , ntRuns =
              [ baseRun{nrLength = 5}
              , baseRun{nrLength = 4, nrStyle = Just StyleMonospaced}
              ]
          }
        `shouldBe` "body\n\n```\ncode\n```"

  describe "noteToMarkdown list styles" $ do
    it "renders StyleBullet 0 as top-level bullet" $
      noteToMarkdown
        NoteText{ntText = "item", ntRuns = [baseRun{nrLength = 4, nrStyle = Just (StyleBullet 0)}]}
        `shouldBe` "- item"

    it "renders StyleBullet 1 indented by 2 spaces" $
      noteToMarkdown
        NoteText{ntText = "nested", ntRuns = [baseRun{nrLength = 6, nrStyle = Just (StyleBullet 1)}]}
        `shouldBe` "  - nested"

    it "renders StyleBullet 2 indented by 4 spaces" $
      noteToMarkdown
        NoteText{ntText = "deep", ntRuns = [baseRun{nrLength = 4, nrStyle = Just (StyleBullet 2)}]}
        `shouldBe` "    - deep"

    it "renders StyleDash 0 as top-level bullet" $
      noteToMarkdown
        NoteText{ntText = "item", ntRuns = [baseRun{nrLength = 4, nrStyle = Just (StyleDash 0)}]}
        `shouldBe` "- item"

    it "separates consecutive list items with a single newline" $
      noteToMarkdown
        NoteText
          { ntText = "first\nsecond"
          , ntRuns =
              [ baseRun{nrLength = 6, nrStyle = Just (StyleBullet 0)}
              , baseRun{nrLength = 6, nrStyle = Just (StyleBullet 0)}
              ]
          }
        `shouldBe` "- first\n- second"

    it "separates a list item from body text with a double newline" $
      noteToMarkdown
        NoteText
          { ntText = "item\nbody"
          , ntRuns =
              [ baseRun{nrLength = 5, nrStyle = Just (StyleBullet 0)}
              , baseRun{nrLength = 4}
              ]
          }
        `shouldBe` "- item\n\nbody"

    it "numbers two consecutive StyleNumbered items" $
      noteToMarkdown
        NoteText
          { ntText = "first\nsecond"
          , ntRuns =
              [ baseRun{nrLength = 6, nrStyle = Just (StyleNumbered 0 Nothing)}
              , baseRun{nrLength = 6, nrStyle = Just (StyleNumbered 0 Nothing)}
              ]
          }
        `shouldBe` "1. first\n2. second"

    it "starts numbered list at ms when ms is Just" $
      noteToMarkdown
        NoteText
          { ntText = "item"
          , ntRuns = [baseRun{nrLength = 4, nrStyle = Just (StyleNumbered 0 (Just 3))}]
          }
        `shouldBe` "3. item"

    it "resets numbered counter after a non-numbered paragraph" $
      noteToMarkdown
        NoteText
          { ntText = "first\nsecond\nbody\nthird"
          , ntRuns =
              [ baseRun{nrLength = 6, nrStyle = Just (StyleNumbered 0 Nothing)}
              , baseRun{nrLength = 7, nrStyle = Just (StyleNumbered 0 Nothing)}
              , baseRun{nrLength = 5}
              , baseRun{nrLength = 5, nrStyle = Just (StyleNumbered 0 Nothing)}
              ]
          }
        `shouldBe` "1. first\n2. second\n\nbody\n\n1. third"

    it "does not reset counter when ms is Just on every item" $
      noteToMarkdown
        NoteText
          { ntText = "first\nsecond"
          , ntRuns =
              [ baseRun{nrLength = 6, nrStyle = Just (StyleNumbered 0 (Just 1))}
              , baseRun{nrLength = 6, nrStyle = Just (StyleNumbered 0 (Just 1))}
              ]
          }
        `shouldBe` "1. first\n2. second"

    it "renders StyleNumbered 1 indented by 2 spaces" $
      noteToMarkdown
        NoteText
          { ntText = "nested"
          , ntRuns = [baseRun{nrLength = 6, nrStyle = Just (StyleNumbered 1 Nothing)}]
          }
        `shouldBe` "  1. nested"

    it "renders StyleChecklist True as checked item" $
      noteToMarkdown
        NoteText
          { ntText = "done"
          , ntRuns = [baseRun{nrLength = 4, nrStyle = Just (StyleChecklist 0 True)}]
          }
        `shouldBe` "- [x] done"

    it "renders StyleChecklist False as unchecked item" $
      noteToMarkdown
        NoteText
          { ntText = "todo"
          , ntRuns = [baseRun{nrLength = 4, nrStyle = Just (StyleChecklist 0 False)}]
          }
        `shouldBe` "- [ ] todo"

  describe "splitIntoParagraphs" $ do
    it "single styled run without newline produces one paragraph" $
      splitIntoParagraphs
        NoteText
          { ntText = "hello"
          , ntRuns = [baseRun{nrLength = 5, nrStyle = Just StyleHeading}]
          }
        `shouldBe` [RawParagraph{rpStyle = Just StyleHeading, rpSegments = [baseSeg "hello"]}]

    it "neutral run is absorbed into the preceding styled paragraph" $
      splitIntoParagraphs
        NoteText
          { ntText = "helloworld"
          , ntRuns =
              [ baseRun{nrLength = 5, nrStyle = Just StyleHeading}
              , baseRun{nrLength = 5}
              ]
          }
        `shouldBe` [ RawParagraph
                       { rpStyle = Just StyleHeading
                       , rpSegments = [baseSeg "hello", baseSeg "world"]
                       }
                   ]

    it "newline in a run closes the current paragraph" $
      splitIntoParagraphs
        NoteText
          { ntText = "a\nb"
          , ntRuns =
              [ baseRun{nrLength = 1, nrStyle = Just StyleHeading}
              , baseRun{nrLength = 1}
              , baseRun{nrLength = 1}
              ]
          }
        `shouldBe` [ RawParagraph{rpStyle = Just StyleHeading, rpSegments = [baseSeg "a"]}
                   , RawParagraph{rpStyle = Nothing, rpSegments = [baseSeg "b"]}
                   ]

    it "inline-only runs with different attributes produce distinct segments in one paragraph" $
      splitIntoParagraphs
        NoteText
          { ntText = "boldnormal"
          , ntRuns = [baseRun{nrLength = 4, nrBold = True}, baseRun{nrLength = 6}]
          }
        `shouldBe` [ RawParagraph
                       { rpStyle = Nothing
                       , rpSegments = [(baseSeg "bold"){rsBold = True}, baseSeg "normal"]
                       }
                   ]

    it "xFFFC with an attachment id is replaced with [attachment: id]" $
      splitIntoParagraphs
        NoteText
          { ntText = "\xFFFC"
          , ntRuns = [baseRun{nrAttachmentId = Just "att-1"}]
          }
        `shouldBe` [RawParagraph{rpStyle = Nothing, rpSegments = [baseSeg "[attachment: att-1]"]}]

    it "xFFFC without an attachment id is replaced with [attachment]" $
      splitIntoParagraphs
        NoteText{ntText = "\xFFFC", ntRuns = [baseRun]}
        `shouldBe` [RawParagraph{rpStyle = Nothing, rpSegments = [baseSeg "[attachment]"]}]


baseRun :: NoteRun
baseRun =
  NoteRun
    { nrLength = 1
    , nrStyle = Nothing
    , nrBold = False
    , nrItalic = False
    , nrUnderline = False
    , nrStrikethrough = False
    , nrAttachmentId = Nothing
    , nrLink = Nothing
    }


baseSeg :: Text -> RawSegment
baseSeg t =
  RawSegment
    { rsText = t
    , rsBold = False
    , rsItalic = False
    , rsStrikethrough = False
    , rsUnderline = False
    , rsLink = Nothing
    }
