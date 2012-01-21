{-# OPTIONS -fno-warn-unused-binds #-}
module Helper.Forms 
    ( presentationForm
    , slideForm
    ) where

import Foundation
import Import
import Yesod.Markdown

presentationForm :: Maybe Presentation -> Form Presentation
presentationForm presentation = renderDivs $ Presentation
  <$> areq textField "Name" (presentationName <$> presentation)

slideForm :: PresentationId -> Maybe Slide -> Form Slide
slideForm presentationId slide = renderDivs $ Slide
  <$> areq textField "Title" (slideTitle <$> slide)
  <*> areq markdownField "Content" (slideContent <$> slide)
  <*> areq intField "Page" (slidePageNumber <$> slide)
  <*> areq intField "Max time" (slideMaxTime <$> slide)
  <*> pure presentationId
