module Handler.Slides 
    ( getSlideR
    , getAllSlidesR
    , postNewSlideR
    , getEditSlideR
    , postEditSlideR
    , getDelSlideR
    ) where

import Foundation
import Import
import Helper.Forms
import Yesod.Markdown

getAllSlidesR :: Handler RepHtml
getAllSlidesR = do
  slides <- runDB $ selectList ([] :: [Filter Slide]) []
  defaultLayout $ do
    setTitle "All slides"
    $(widgetFile "slides_index")

getSlideR :: SlideId -> Handler RepHtml
getSlideR slideId = do
  slide <- runDB $ get404 slideId
  allSlides <- runDB $  selectList [SlidePresentationId ==. slidePresentationId slide] []
  let slidePageMax = maximum $ fmap (slidePageNumber . snd) allSlides
  nextSlide <- runDB $ selectFirst [SlidePageNumber ==. (slidePageNumber slide) + 1
                                   , SlidePresentationId ==. (slidePresentationId slide)
                                   ] []
  prevSlide <- runDB $ selectFirst [SlidePageNumber ==. (slidePageNumber slide) - 1
                                   , SlidePresentationId ==. (slidePresentationId slide)
                                   ] []
  defaultLayout $ do
    setTitle "getSlide"
    $(widgetFile "slide")

postNewSlideR :: PresentationId -> Handler RepHtml
postNewSlideR presentationId = do
  ((result, _), _) <- runFormPost (slideForm presentationId Nothing)
  case result of
    FormSuccess slide -> do
      _ <- runDB $ insert slide
      setMessage "Successfully added slide"
    _ -> do
      setMessage "Could not add slide"
  redirect RedirectTemporary (EditPresentationR presentationId)

getEditSlideR :: SlideId -> Handler RepHtml
getEditSlideR slideId = do
  slide <- runDB $ get404 slideId
  ((_, formContent), enctype) <- generateFormPost (slideForm (slidePresentationId slide) (Just slide))
  defaultLayout $ do
    setTitle "getEditSlide"
    $(widgetFile "edit_slide")


postEditSlideR :: SlideId -> Handler RepHtml
postEditSlideR slideId = do
  slide <- runDB $ get404 slideId
  ((result, _), _) <- runFormPost (slideForm (slidePresentationId slide) Nothing)
  case result of
    FormSuccess slideFormSuccess -> do
      runDB $ update slideId
        [ SlideTitle =. slideTitle slideFormSuccess
        , SlideContent =. slideContent slideFormSuccess
        , SlidePageNumber =. slidePageNumber slideFormSuccess
        , SlideMaxTime =. slideMaxTime slideFormSuccess
        ]
      setMessage "Slide successfully edited"
    _ -> do
      setMessage "Could not edit slide"
  redirect RedirectTemporary (EditPresentationR (slidePresentationId slide))

getDelSlideR :: SlideId -> Handler RepHtml
getDelSlideR slideId = do
  slide <- runDB $ get404 slideId
  runDB $ delete slideId
  setMessage "Slide successfully deleted"
  redirect RedirectTemporary (EditPresentationR (slidePresentationId slide))
