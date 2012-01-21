module Handler.Presentations 
    ( getAllPresentationsR
    , postNewPresentationR
    , getPresentationR
    , getEditPresentationR
    , postEditPresentationR
    , getDelPresentationR
    ) where

import Foundation
import Import
import Helper.Forms

getAllPresentationsR :: Handler RepHtml
getAllPresentationsR = do
  ((_, formContent), enctype) <- generateFormPost (presentationForm Nothing)
  presentations <- runDB $ selectList ([] :: [Filter Presentation]) []
  defaultLayout $ do
    setTitle "All slides"
    $(widgetFile "presentations_index")

postNewPresentationR :: Handler RepHtml
postNewPresentationR = do
  ((result, _), _) <- runFormPost (presentationForm Nothing)
  case result of
    FormSuccess presentation -> do
      presentationId <- runDB $ insert presentation
      redirect RedirectTemporary (PresentationR presentationId)
    _ -> do
      setMessage "Could not add presentation"
      redirect RedirectTemporary AllPresentationsR

getPresentationR :: PresentationId -> Handler RepHtml
getPresentationR presentationId = do
  presentation <- runDB $ get404 presentationId
  slides <- runDB $  selectList [SlidePresentationId ==. presentationId] []
  defaultLayout $ do
    setTitle "getPresentation"
    $(widgetFile "presentation")

getEditPresentationR :: PresentationId -> Handler RepHtml
getEditPresentationR presentationId = do
  presentation <- runDB $ get404 presentationId
  slides <- runDB $  selectList [SlidePresentationId ==. presentationId] []
  ((_, formContentPresentation), enctypePresentation) <- generateFormPost (presentationForm $ Just presentation)
  ((_, formContentSlide), enctypeSlide) <- generateFormPost (slideForm presentationId Nothing)
  defaultLayout $ do
    setTitle "getEditPresentation"
    $(widgetFile "edit_presentation")

postEditPresentationR :: PresentationId -> Handler RepHtml
postEditPresentationR presentationId = do
  ((result, _), _) <- runFormPost (presentationForm Nothing)
  case result of
    FormSuccess presentation -> do
      runDB $ update presentationId
        [PresentationName =. presentationName presentation
        ]
      setMessage "presentation successfully edited"
    _ -> do
      setMessage "Could not edit presentation"
  redirect RedirectTemporary AllPresentationsR

getDelPresentationR :: PresentationId -> Handler RepHtml
getDelPresentationR presentationId = do
  runDB $ delete presentationId
  setMessage "Presentation successfully deleted"
  redirect RedirectTemporary AllPresentationsR
