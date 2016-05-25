module PostSearch exposing ( filterPosts )

import String exposing ( contains )

import Post exposing (..)

filterPosts : (List Post) -> String -> (List Post)
filterPosts posts searchTerm = List.filter (\post -> contains searchTerm post.content) posts
--filterArticles articles searchTerm = articles
