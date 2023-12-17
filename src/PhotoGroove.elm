module PhotoGroove exposing (main)

import Browser
import Html exposing (div, h1, img, text, Html)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)


type alias Photo =
  { url : String }

type alias Model =
  { photos : List Photo
  , selectedUrl : String
  }

type alias Message =
  { description : String, data : String }


urlPrefix : String
urlPrefix = "https://elm-in-action.com/"

view : Model -> Html Message
view model = 
  div [ class "content" ]
    [ h1 [] [text "Photo Groove" ]
    , div [ id "thumbnails" ]
      (List.map (viewThumbnail model.selectedUrl) model.photos)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ model.selectedUrl)
        ]
        []
    ]


selectPhoto : Message
selectPhoto = { description = "ClickedPhoto", data = "1.jpeg" }


viewThumbnail : String -> Photo -> Html Message
viewThumbnail selectedUrl thumb =
  img
    [ src (urlPrefix ++ thumb.url)
    , classList [ ( "selected", selectedUrl == thumb.url ) ]
    , onClick { selectPhoto | data = thumb.url }
    ]
    []


initialModel : Model
initialModel =
  { photos =
    [ { url = "1.jpeg" }
    , { url = "2.jpeg" }
    , { url = "3.jpeg" }
    , { url = "4.jpeg" }
    ]
    , selectedUrl = "1.jpeg"
  }


photoArray : Array Photo
photoArray = Array.fromList initialModel.photos


update : Message -> Model -> Model
update msg model =
  if msg.description == "ClickedPhoto" then
    { model | selectedUrl = msg.data }

  else
    model


main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
