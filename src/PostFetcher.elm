module PostFetcher exposing ( fetchPosts )

import Task exposing (..)
import Json.Decode as Json exposing ((:=))
import Http

import Post exposing (..)

fetchPosts : String -> Task Http.Error (List Post)
fetchPosts url =  Http.get posts url

posts : Json.Decoder (List Post)
posts =
    let article =
        Json.object3 Post
          ("relativeUrl" := Json.string)
          ("title"       := Json.string)
          ("content"     := Json.string)
    in "posts" := Json.list article
