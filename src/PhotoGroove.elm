module PhotoGroove exposing (main)

import Browser
import Html exposing
  ( div
  , h1
  , h3
  , img
  , text
  , label
  , input
  , button
  , Html
  )

import Html.Attributes exposing
  ( classList
  , class
  , src
  , id
  , name
  , type_
  , title
  )

import Html.Events exposing (onClick)
import Random
import Http
import Json.Decode exposing (Decoder, int, list, string, int, succeed)
import Json.Decode.Pipeline exposing (optional, required)


type alias Photo =
  { url : String
  , size : Int
  , title : String
  }

type Status
  = Loading
  | Loaded (List Photo) String
  | Errored String

type alias Model =
  { status : Status
  , chosenSize : ThumbnailSize
  }

type Message
  = ClickedPhoto String
  | ClickedSize ThumbnailSize
  | ClickedSurpriseMe
  | GotRandomPhoto Photo
  | GotPhotos (Result Http.Error (List Photo))


type ThumbnailSize
  = Small
  | Medium
  | Large


urlPrefix : String
urlPrefix = "https://elm-in-action.com/"

initialModel : Model
initialModel =
  { status = Loading
  , chosenSize = Medium
  }


view : Model -> Html Message
view model = 
  div [ class "content" ] <|
    case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model.chosenSize
      
      Loading -> []

      Errored errorMessage ->
        [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Message)
viewLoaded photos selectedUrl chosenSize =
  [ h1 [] [ text "Photo Groove" ]
  , button
    [ onClick ClickedSurpriseMe ]
    [ text "Surprise Me!" ]
  , h3 [] [ text "Thumbnail Size:" ]
  , div [ id "choose-size" ]
    (List.map viewSizeChooser [ Small, Medium, Large ])
  , div [ id "thumbnails", class (sizeToString chosenSize) ]
    (List.map (viewThumbnail selectedUrl) photos)
  , img
      [ class "large"
      , src (urlPrefix ++ "large/" ++ selectedUrl)
      ]
      []
  ]


viewThumbnail : String -> Photo -> Html Message
viewThumbnail selectedUrl thumb =
  img
    [ src (urlPrefix ++ thumb.url)
    , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
    , classList [ ( "selected", selectedUrl == thumb.url ) ]
    , onClick (ClickedPhoto thumb.url)
    ]
    []


viewSizeChooser : ThumbnailSize -> Html Message
viewSizeChooser size =
  label []
    [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
    , text (sizeToString size)
    ]


sizeToString : ThumbnailSize -> String
sizeToString size =
  case size of
    Small -> "small"
    Medium -> "med"
    Large -> "large"


selectUrl : String -> Status -> Status
selectUrl url status =
 case status of
  Loaded photos _ ->
    Loaded photos url

  Loading -> status

  Errored _ -> status 


update : Message -> Model -> (Model, Cmd Message)
update msg model =
  case msg of
    ClickedPhoto url ->
      ( { model | status = selectUrl url model.status }, Cmd.none )

    ClickedSize size ->
      ( { model | chosenSize = size }, Cmd.none )

    ClickedSurpriseMe ->
      case model.status of
        Loaded (firstPhoto :: otherPhotos) _ ->
          Random.uniform firstPhoto otherPhotos
            |> Random.generate GotRandomPhoto
            |> Tuple.pair model

        Loaded [] _ -> ( model, Cmd.none )
        
        Loading -> (model, Cmd.none)

        Errored _ -> (model, Cmd.none)

    GotRandomPhoto photo ->
      ( { model | status = selectUrl photo.url model.status }
      , Cmd.none
      )

    GotPhotos (Ok photos) ->
      case photos of
        first :: rest ->
          ( { model | status = Loaded photos first.url }
          , Cmd.none
          )

        [] ->
          ( { model | status = Errored "0 photos found" }, Cmd.none )

    GotPhotos (Err _) ->
      ( { model | status = Errored "server error!" }, Cmd.none )


photoDecoder : Decoder Photo
photoDecoder =
  succeed Photo
    |> required "url" string
    |> required "size" int
    |> optional "title" string "(untitled)"

initialCmd : Cmd Message
initialCmd =
  Http.get
    { url = "https://elm-in-action.com/photos/list.json"
    , expect = Http.expectJson GotPhotos (list photoDecoder)
    }


main : Program () Model Message
main =
  Browser.element
    { init = \_ -> ( initialModel, initialCmd )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
