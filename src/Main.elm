port module Main exposing (..)

import Base64
import Browser exposing (Document)
import Bytes exposing (Bytes)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (on, onClick)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (Decoder)
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Svg.Lazy as Svg
import Task
import Time


port mouseDrag : (MousePosition -> msg) -> Sub msg


port startDrag : () -> Cmd msg


port wheel : (Float -> msg) -> Sub msg


port startWheel : () -> Cmd msg


port endWheel : () -> Cmd msg

port resize : (BoundingClientRect -> msg) -> Sub msg

port canvasSize : () -> Cmd msg

type alias BoundingClientRect =
    { x : Float
    , y : Float
    , width: Float
    , height: Float
    , top : Float
    }
-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
-- The app needs to scale the svg with dom changes, but for now lets hardcode


type alias Model =
    { raster : Maybe RasterImage
    , transform : Transform
    , dragging : Bool
    , lastMousePosition : MousePosition
    , canvasBoundingClientRect : BoundingClientRect
    }


type alias Transform =
    { x : Float
    , y : Float
    , z : Float
    }


defaultTransform : Transform
defaultTransform =
    { x = 0, y = 0, z = 1 }


translateTransform : Transform -> Float -> Float -> Transform
translateTransform t x y =
    { t | x = t.x + x, y = t.y + y }


zoomTransform : Transform -> Float -> Transform
zoomTransform t z =
    { t | z = t.z + z }


transformToViewBox : Transform -> String
transformToViewBox t =
    String.join " " [ String.fromFloat t.x, String.fromFloat t.y, String.fromFloat <| 5000 + t.z, String.fromFloat <| 5000 + t.z ]


defaultMousePosition : MousePosition
defaultMousePosition =
    { timeStamp = 0, offsetX = 0, offsetY = 0 }

defaultBoundingClientRect : BoundingClientRect
defaultBoundingClientRect =
    { x = 0, y = 0, width = 500, height = 500, top = 0}

type alias RasterImage =
    { mime : String
    , b64data : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing defaultTransform False defaultMousePosition defaultBoundingClientRect, Cmd.none )



-- UPDATE


type Msg
    = GetImage
    | GotImage File
    | GotImageBytes ( String, Bytes )
    | MouseDown MousePosition
    | MouseMove MousePosition
    | MouseOver
    | MouseOut
    | MouseWheel Float
    | Resize BoundingClientRect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetImage ->
            ( model
            , Select.file [ "image/jpeg", "image/png" ] GotImage
            )

        GotImage file ->
            ( model
            , Task.perform GotImageBytes (File.toBytes file |> Task.map (\bytes -> ( File.mime file, bytes )))
            )

        GotImageBytes ( mime, bytes ) ->
            case Base64.fromBytes bytes of
                Just b64 ->
                    ( { model | raster = Just { mime = mime, b64data = b64 } }
                    , canvasSize ()
                    )

                Nothing ->
                    ( model, Cmd.none )

        MouseDown mousePosition ->
            ( { model | lastMousePosition = mousePosition }, startDrag () )

        MouseMove mousePosition ->
            let
                dx =
                    model.lastMousePosition.offsetX - mousePosition.offsetX

                dy =
                    model.lastMousePosition.offsetY - mousePosition.offsetY
            in
            ( { model | lastMousePosition = mousePosition, transform = translateTransform model.transform dx dy }, Cmd.none )

        MouseOver ->
            ( model, startWheel () )

        MouseOut ->
            ( model, endWheel () )

        MouseWheel delta ->
            ( { model | transform = zoomTransform model.transform delta }, Cmd.none )

        Resize boundingClientRect ->
            -- TODO store this somewhere
            ({model | canvasBoundingClientRect = boundingClientRect}, Cmd.none)



-- VIEW


view : Model -> Document Msg
view model =
    { title = "HEllo"
    , body =
        [ case model.raster of
            Nothing ->
                button [ onClick GetImage ] [ text "Load CSV" ]

            Just content ->
                lazy
                    (\m ->
                        div
                            [ id "root"
                            ]
                            [ div [ class "flex-row" ]
                                [ div [ id "canvas-container" ]
                                    [ Svg.svg
                                        [ SvgA.id "canvas"
                                        , SvgE.preventDefaultOn "mousedown" (Decode.map (\msg -> ( msg, True )) (Decode.map MouseDown decodeMousePosition))
                                        , SvgE.onMouseOut MouseOver
                                        , SvgE.onMouseOut MouseOut
                                        , SvgA.viewBox <| transformToViewBox model.transform
                                        ]
                                        [ Svg.lazy (\c -> Svg.image [ SvgA.xlinkHref <| "data:" ++ c.mime ++ ";base64," ++ c.b64data ] []) content
                                        ]
                                    ]
                                ]
                            ]
                    )
                    model

        --p [ style "white-space" "pre" ] [ text <| Debug.toString content ]
        ]
    }


type alias MousePosition =
    { timeStamp : Float
    , offsetX : Float
    , offsetY : Float
    }


decodeMousePosition : Decoder MousePosition
decodeMousePosition =
    Decode.map3 MousePosition
        (Decode.field "timeStamp" Decode.float)
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ mouseDrag MouseMove
        , wheel MouseWheel
        , resize Resize
        ]
