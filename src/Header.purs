module Header where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Class

import Control.Monad.Aff
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except

import Data.Array

import Data.List.Types (NonEmptyList)
import Data.Foreign
import Data.Foreign.Generic
import Data.Foreign.JSON

import Data.Maybe
import Data.Either
import Data.Foreign (MultipleErrors)

import Network.HTTP.Affjax (get, post, AJAX, AffjaxResponse)

import React as R
import React.DOM as R
import React.DOM.Props as RP
import Simple.JSON
import Thermite as T
import Unsafe.Coerce

import Stock

-- data HeaderState = HeaderState
-- data HeaderAction = HeaderAction

-- http://blog.functorial.com/posts/2015-11-20-Thermite.html

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

-- https://leanpub.com/purescript/read#leanpub-auto-working-with-untyped-data
-- type F = Except (NonEmptyList ForeignError)

stockQuery :: forall e. String -> Aff (ajax :: AJAX, console :: CONSOLE | e) (Maybe Stock)
stockQuery query = do
  res <- get ("http://localhost:1234/tickerQuery?q="<>query)
  liftEff $ log $ "response: " <> res.response

  let
    eForeign :: Either (NonEmptyList ForeignError) Foreign
    eForeign = runExcept (parseJSON res.response)

    eParsed :: Either (NonEmptyList ForeignError) Stock
    eParsed = runExcept (decodeJSON res.response)

  pure $ hush eParsed


correlatedQuery :: forall e. String -> Aff (ajax :: AJAX, console :: CONSOLE | e) (Array Stock)
correlatedQuery query = do
  liftEff $ log $ "query: "<>query
  let uri = "http://localhost:1234/correlated?q="<>query
  liftEff $ log uri
  res <- get uri 
  let eParsed :: Either (NonEmptyList ForeignError) (Array Stock)
      eParsed = runExcept (decodeJSON res.response)

      parsed :: Array Stock
      parsed = either (\_ -> []) id eParsed

  pure parsed

header :: T.Spec _ AppState _ AppAction
header = T.simpleSpec performAction render
  where
    performAction :: T.PerformAction _ AppState _ AppAction
    performAction (SubmitQuery query') _ _ = do
      mstock' <- lift $ stockQuery query'
      correlated' <- lift $ correlatedQuery query'
      void $ T.modifyState (\appState ->
                             appState {
                               query = query'
                               , mstock = mstock'
                               })
      void $ T.modifyState (\appState ->
                             appState {
                               correlated = correlated'
                               }
                           )
       
    render :: T.Render _ _ _
    render dispatch _ state _ =
      [ R.h1' [R.text "Header" ]
      , R.p' [ R.text "Search for a stock by name or ticker symbol" ]
      , R.p' [
           R.input [
              RP.onChange \e ->
               dispatch (SubmitQuery (unsafeEventValue e))
              ] []
           , R.button [
              RP.onClick \_ ->
               dispatch (SubmitQuery (state.query))
              ] [ R.text "Submit" ]
           ]
      ] <> case state.mstock of
        (Just (Stock { tickerSymbol, description })) ->
          [ R.p' [ R.text tickerSymbol, R.text ": ", R.text description ] ]
        Nothing -> []

-- http://try.purescript.org/?gist=f5f273e4c5e4161fceff&backend=thermite&session=87ef2d8b-4fab-2175-2a20-38042f523ff7

