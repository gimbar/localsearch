module Post exposing (Post)

type alias Post =
    { relativeUrl : String
    , title       : String
    , content     : String}
