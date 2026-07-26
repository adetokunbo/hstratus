{-# LANGUAGE OverloadedStrings #-}

module HStratus.Notes.MarkdownSpec (spec) where

import Data.Text (Text)
import Network.HStratus.Internal.Notes.Markdown
import Network.HStratus.Internal.Notes.Note
import Test.Hspec


spec :: Spec
spec = describe "splitIntoParagraphs" $ do
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
      `shouldBe`
      [ RawParagraph
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
      `shouldBe`
      [ RawParagraph{rpStyle = Just StyleHeading, rpSegments = [baseSeg "a"]}
      , RawParagraph{rpStyle = Nothing, rpSegments = [baseSeg "b"]}
      ]

  it "inline-only runs with different attributes produce distinct segments in one paragraph" $
    splitIntoParagraphs
      NoteText
        { ntText = "boldnormal"
        , ntRuns = [baseRun{nrLength = 4, nrBold = True}, baseRun{nrLength = 6}]
        }
      `shouldBe`
      [ RawParagraph
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
      `shouldBe`
      [RawParagraph{rpStyle = Nothing, rpSegments = [baseSeg "[attachment: att-1]"]}]

  it "xFFFC without an attachment id is replaced with [attachment]" $
    splitIntoParagraphs
      NoteText{ntText = "\xFFFC", ntRuns = [baseRun]}
      `shouldBe`
      [RawParagraph{rpStyle = Nothing, rpSegments = [baseSeg "[attachment]"]}]


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
