port module PhotoGroove exposing (main)

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
  , node
  , canvas
  , Html
  )

import Html.Attributes as Attr exposing
  ( classList
  , class
  , src
  , id
  , name
  , property
  , type_
  , title
  )

import Html.Events exposing (on, onClick)
import Random
import Http
import Json.Encode as Encode
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Html exposing (Attribute)
import Platform.Cmd as Cmd
import Browser.Dom exposing (getElement)


port setFilters : FilterOptions -> Cmd msg
port activityChanges : (String -> msg) -> Sub msg

type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }

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
  , activity : String
  , chosenSize : ThumbnailSize
  , hue : Int
  , ripple : Int
  , noise : Int
  }

type Msg
  = ClickedPhoto String
  | SlidHue Int
  | SlidRipple Int
  | SlidNoise Int
  | ClickedSize ThumbnailSize
  | ClickedSurpriseMe
  | GotRandomPhoto Photo
  | GotActivity String
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
  , activity = ""
  , chosenSize = Medium
  , hue = 0
  , ripple = 0
  , noise = 0
  }


view : Model -> Html Msg
view model = 
  div [ class "content" ] <|
    case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model
      
      Loading -> []

      Errored errorMessage ->
        [ text ("Error: " ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
  div [ class "filter-slider" ]
    [ label [] [ text name ]
    , rangeSlider
      [ Attr.max "11"
      , property "val" (Encode.int magnitude)
      , onSlide toMsg
      ]
      []
    , label [] [ text (String.fromInt magnitude) ]
    ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
  [ h1 [] [ text "Photo Groove" ]
  , button
    [ onClick ClickedSurpriseMe ]
    [ text "Surprise Me!" ]
  , div [ class "activity" ] [ text model.activity ]
  , div [ class "filters" ]
    [ viewFilter SlidHue "Hue" model.hue
    , viewFilter SlidRipple "Ripple" model.ripple
    , viewFilter SlidNoise "Noise" model.noise
    ]
  , h3 [] [ text "Thumbnail Size:" ]
  , div [ id "choose-size" ]
    (List.map viewSizeChooser [ Small, Medium, Large ])
  , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
    (List.map (viewThumbnail selectedUrl) photos)
  , canvas [ id "main-canvas", class "large" ] []
  ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
  img
    [ src (urlPrefix ++ thumb.url)
    , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
    , classList [ ( "selected", selectedUrl == thumb.url ) ]
    , onClick (ClickedPhoto thumb.url)
    ]
    []



viewSizeChooser : ThumbnailSize -> Html Msg
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickedPhoto url ->
      applyFilters { model | status = selectUrl url model.status }

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
      applyFilters { model | status = selectUrl photo.url model.status }

    GotPhotos (Ok photos) ->
      case photos of
        first :: _ ->
          applyFilters { model | status = Loaded photos first.url }
          -- applyFilters { model | status =
          --       case List.head photos of
          --         Just photo ->
          --           Loaded photos photo.url
                  
          --         Nothing ->
          --           Loaded [] ""
          --   }

        [] ->
          ( { model | status = Errored "0 photos found" }, Cmd.none )

    GotPhotos (Err _) ->
      ( { model | status = Errored "server error!" }, Cmd.none )

    SlidHue hue ->
      applyFilters { model | hue = hue }

    SlidRipple ripple ->
      applyFilters { model | ripple = ripple }

    SlidNoise noise ->
      applyFilters { model | noise = noise }

    GotActivity activity ->
      ( { model | activity = activity }, Cmd.none )


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
  case model.status of
    Loaded _ selectedUrl ->
      let
        filters =
          [ { name = "Hue", amount = toFloat model.hue / 11 }
          , { name = "Ripple", amount = toFloat model.ripple / 11 }
          , { name = "Noise", amount = toFloat model.noise / 11 }
          ]

        url =
          urlPrefix ++ "large/" ++ selectedUrl

      in
      ( model, setFilters { url = url, filters = filters } )
    
    Loading ->
      ( model, Cmd.none )
    
    Errored _ ->
      ( model, Cmd.none )
    

photoDecoder : Decoder Photo
photoDecoder =
  succeed Photo
    |> required "url" string
    |> required "size" int
    |> optional "title" string "(untitled)"

initialCmd : Cmd Msg
initialCmd =
  Http.get
    { url = "https://elm-in-action.com/photos/list.json"
    , expect = Http.expectJson GotPhotos (list photoDecoder)
    }


main : Program Float Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> activityChanges GotActivity
    }


init : Float -> ( Model, Cmd Msg )
init flags =
  let
    activity =
      "initializing pasta v" ++ String.fromFloat flags
  in
  ( { initialModel | activity = activity }, initialCmd )


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
  node "range-slider" attributes children


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
  at [ "detail", "userSlidTo" ] int
    |> Json.Decode.map toMsg
    |> on "slide"
    
