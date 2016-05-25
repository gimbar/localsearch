module Main exposing (..)

import Task exposing (..)
import Http
import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events
import Html.App as App

import Post exposing (..)
import PostFetcher exposing (..)
import PostSearch exposing (..)

main : Program Never
main =
  App.program
    { init = init
    , subscriptions = subscriptions
    , view = view
    , update = update }

type Model = ErrorModel Http.Error
           | SearchModel
                { posts : List Post
                , searchTerm : String
                , searchResult : List Post
                }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

emptyModel : Model
emptyModel = SearchModel
            { posts = []
            , searchTerm = ""
            , searchResult = []}

type Msg = NoOp
         | UpdateSearchTerm String
         | LoadPosts (List Post)
         | FetchFailed Http.Error

init : (Model, Cmd Msg)
init = (emptyModel, getPosts)

getPosts : Cmd Msg
getPosts =
    let task = fetchPosts "http://localhost:8000/posts.json"
    in Task.perform FetchFailed LoadPosts task

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case model of
        ErrorModel lastError ->
            case message of
                FetchFailed error -> ( ErrorModel error, Cmd.none )
                _                 -> ( ErrorModel lastError, Cmd.none )
        SearchModel searchModel ->
            case message of
                NoOp ->
                    ( model, Cmd.none )
                UpdateSearchTerm newSearchTerm ->
                    ( SearchModel
                        { searchModel
                        | searchTerm = newSearchTerm
                        , searchResult = (filterPosts searchModel.posts newSearchTerm)}
                    , Cmd.none )
                LoadPosts posts ->
                    ( SearchModel
                        { searchModel
                        | posts = posts
                        , searchResult = (filterPosts posts searchModel.searchTerm)}
                    , Cmd.none )
                FetchFailed error ->
                    ( ErrorModel error, Cmd.none )

view : Model -> Html.Html Msg
view model =
    case model of
        ErrorModel error ->
            Html.div [] [Html.text (toString error)]
        SearchModel searchModel ->
            Html.div []
                [ Html.input [ Attributes.placeholder "search", Events.onInput UpdateSearchTerm ] []
                , Html.div [] [Html.text (toString (List.length searchModel.searchResult))]
                ]
