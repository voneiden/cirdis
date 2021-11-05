port module Main exposing (..)

import Base64
import Browser exposing (Document)
import Browser.Events exposing (onKeyDown, onMouseMove, onMouseUp)
import Bytes exposing (Bytes)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, id, placeholder, value)
import Html.Events exposing (keyCode, onClick)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Svg.Lazy as Svg
import Task



-- Todo things
-- make zoom more sane (=multiplier)


port mouseDrag : (MousePosition -> msg) -> Sub msg


port startDrag : () -> Cmd msg


port wheel : (Float -> msg) -> Sub msg


port startWheel : () -> Cmd msg


port endWheel : () -> Cmd msg


port resize : (BoundingClientRect -> msg) -> Sub msg


port canvasSize : () -> Cmd msg


port checkImages : () -> Cmd msg


port imageInformation : (List ImageInformation -> msg) -> Sub msg


type alias BoundingClientRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , top : Float
    }


type alias ImageInformation =
    { layer : String
    , height : Float
    , width : Float
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


type LayerError
    = RasterB64EncodeFailed


type alias LayerData =
    RemoteData LayerError LayerDefinition


type alias Model =
    { loadLayerStatus : LayerData
    , layers : List LayerDefinition
    , active : Maybe Int
    , transform : Transform
    , dragging : Bool
    , lastMousePosition : MousePosition
    , canvasBoundingClientRect : BoundingClientRect
    , trace : List Point
    }


type alias Point =
    { x : Float
    , y : Float
    }


type Shape
    = Trace
    | Zone
    | Pad
    | Via


type alias Transform =
    { x : Float
    , y : Float
    , z : Float
    }


defaultTransform : Transform
defaultTransform =
    { x = 0, y = 0, z = 1 }


{-| Do panning translations to the transformation.

Panning should happen with the same speed as the mouse
moves on the screen, therefore the magnitude is tied to the
zoom multiplier.

-}
translateTransform : BoundingClientRect -> Transform -> Float -> Float -> Transform
translateTransform r t x y =
    { t | x = t.x + x * t.z, y = t.y + y * t.z }


{-| Apply zoom to transform. While it accepts a float,
actually it only cares about the sign. This is due to
differences with browser wheel event delta's.
-}
zoomTransform : Transform -> Float -> Transform
zoomTransform t z =
    let
        multiplier =
            1.25

        newZ =
            if z > 0 then
                t.z * multiplier

            else
                t.z / multiplier
    in
    if newZ > 0.9 && newZ < 1.1 then
        { t | z = 1 }

    else
        { t | z = newZ }


{-| Convert a Transform into a SVG viewBox attribute value
-}
transformToViewBox : BoundingClientRect -> Transform -> String
transformToViewBox r t =
    let
        width =
            r.width * t.z

        height =
            r.height * t.z
    in
    String.join " " [ String.fromFloat <| t.x - (width / 2), String.fromFloat <| t.y - (height / 2), String.fromFloat <| width, String.fromFloat <| height ]


defaultMousePosition : MousePosition
defaultMousePosition =
    { timeStamp = 0, offsetX = 0, offsetY = 0, button = 0 }


defaultBoundingClientRect : BoundingClientRect
defaultBoundingClientRect =
    { x = 0, y = 0, width = 500, height = 500, top = 0 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model RemoteData.NotAsked [] Nothing defaultTransform False defaultMousePosition defaultBoundingClientRect [], Cmd.none )



-- UPDATE


type alias LayerDefinition =
    { title : String
    , mimeType : String
    , b64Data : String
    , opacity : Int
    }


type alias FileInfo =
    { name : String
    , mime : String
    , data : Bytes
    }


type Msg
    = GetLayerImage
    | GotLayerImage File
    | GotLayerDefinition FileInfo
    | RemoveLayer Int
    | MouseDown MousePosition
    | MouseMove MousePosition
    | MouseUp MousePosition
    | MouseOver
    | MouseOut
    | MouseWheel Float
    | Resize BoundingClientRect
    | GotImageInformation (List ImageInformation)
    | KeyDown Int


{-| Take the first element of a list and move it as the last element

If you wanna go the other way, use List.reverse I guess

-}
cycle : List a -> List a
cycle list =
    case list of
        a :: b ->
            b ++ [ a ]

        [] ->
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetLayerImage ->
            ( model
            , Select.file [ "image/jpeg", "image/png" ] GotLayerImage
            )

        GotLayerImage file ->
            ( model
            , Task.perform GotLayerDefinition (File.toBytes file |> Task.map (\bytes -> { name = File.name file, mime = File.mime file, data = bytes }))
            )

        GotLayerDefinition fileInfo ->
            case Base64.fromBytes fileInfo.data of
                Just b64 ->
                    let
                        layerDefinition =
                            { title = fileInfo.name
                            , mimeType = fileInfo.mime
                            , b64Data = b64
                            , opacity = 100
                            }
                    in
                    ( { model | layers = layerDefinition :: model.layers }
                    , Cmd.batch [ canvasSize (), checkImages () ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemoveLayer index ->
            ( { model | layers = List.take index model.layers ++ List.drop (index + 1) model.layers }, Cmd.none )

        MouseDown mousePosition ->
            -- Middle mouse button starts drag
            case mousePosition.button of
                1 ->
                    ( { model | lastMousePosition = mousePosition, dragging = True }, Cmd.none )

                0 ->
                    ( { model | trace = model.trace ++ [ mousePositionToPoint model.canvasBoundingClientRect model.transform mousePosition ] }, startDrag () )

                _ ->
                    ( model, Cmd.none )

        MouseUp mousePosition ->
            case mousePosition.button of
                1 ->
                    ( { model | dragging = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseMove mousePosition ->
            if model.dragging then
                let
                    dx =
                        model.lastMousePosition.offsetX - mousePosition.offsetX

                    dy =
                        model.lastMousePosition.offsetY - mousePosition.offsetY
                in
                ( { model | lastMousePosition = mousePosition, transform = translateTransform model.canvasBoundingClientRect model.transform dx dy }, Cmd.none )

            else
                ( { model | lastMousePosition = mousePosition }, Cmd.none )

        MouseOver ->
            ( model, startWheel () )

        MouseOut ->
            ( model, endWheel () )

        MouseWheel delta ->
            ( { model | transform = zoomTransform model.transform delta }, Cmd.none )

        Resize boundingClientRect ->
            -- TODO store this somewhere
            ( { model | canvasBoundingClientRect = boundingClientRect }, Cmd.none )

        GotImageInformation imageInformations ->
            case List.take 1 imageInformations of
                info :: _ ->
                    let
                        -- Frame the layer nicely
                        widthRatio =
                            info.width / model.canvasBoundingClientRect.width

                        heightRatio =
                            info.height / model.canvasBoundingClientRect.height
                    in
                    ( { model
                        | transform =
                            { x = info.width / 2
                            , y = info.height / 2
                            , z = max widthRatio heightRatio + 0.1
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        KeyDown keyCode ->
            case keyCode of
                86 ->
                    ( { model | layers = cycle model.layers }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Circuit Dissector"
    , body =
        [ lazy
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
                                , SvgA.viewBox <| transformToViewBox model.canvasBoundingClientRect model.transform
                                , SvgA.preserveAspectRatio "slice"
                                ]
                                [ Maybe.withDefault (text "") <| Maybe.map viewLayer <| List.head model.layers
                                , viewTrace model.canvasBoundingClientRect model.transform model.lastMousePosition model.trace
                                ]
                            ]
                        , div [ id "right-menu" ]
                            [ viewLayerList model.layers
                            , viewLayerSelect "changeit" model.loadLayerStatus
                            ]
                        ]
                    ]
            )
            model
        ]
    }


viewLayer : LayerDefinition -> Svg Msg
viewLayer layer =
    if layer.opacity == 0 then
        Svg.text ""

    else
        Svg.lazy
            (\l ->
                Svg.image
                    [ SvgA.class <| "layer-" ++ l.title
                    , SvgA.xlinkHref <| "data:" ++ l.mimeType ++ ";base64," ++ l.b64Data
                    ]
                    []
            )
            layer


fromPoint : String -> Point -> String
fromPoint cmd point =
    cmd ++ " " ++ String.fromFloat point.x ++ "," ++ String.fromFloat point.y


mousePositionToPoint : BoundingClientRect -> Transform -> MousePosition -> Point
mousePositionToPoint b t mousePosition =
    let
        deltaX = mousePosition.offsetX - b.width / 2
        deltaY = mousePosition.offsetY - b.height / 2
    in
    { x = t.x + deltaX * t.z, y = t.y + deltaY * t.z }


viewTrace : BoundingClientRect -> Transform -> MousePosition -> List Point -> Svg Msg
viewTrace b t mousePosition points =
    case points of
        start :: rest ->
            let
                d =
                    SvgA.d <|
                        String.join " " <|
                            [ fromPoint "M" start ]
                                ++ List.map (fromPoint "L") rest
                                ++ [ fromPoint "L" (mousePositionToPoint b t mousePosition) ]
            in
            Svg.path [ d, SvgA.fill "none", SvgA.stroke "red" ] []

        _ ->
            Svg.text ""


viewLayerList : List LayerDefinition -> Html Msg
viewLayerList layers =
    div [ class "layer-list" ] <|
        List.indexedMap viewLayerControls layers


viewLayerControls : Int -> LayerDefinition -> Html Msg
viewLayerControls index layer =
    div [ class "layer-info" ]
        [ span [] [ text layer.title ]
        , span [ onClick <| RemoveLayer index ] [ text "[Remove]" ]
        ]


viewLayerSelect : String -> LayerData -> Html Msg
viewLayerSelect title layerData =
    case layerData of
        RemoteData.NotAsked ->
            div []
                [ --input [ placeholder "New Layer", value title ] []
                  button [ onClick <| GetLayerImage ] [ text <| "Import layer" ]
                ]

        RemoteData.Loading ->
            div []
                [ text "Loading.." ]

        RemoteData.Failure error ->
            div []
                [ text "Error todo" ]

        -- Todo shouldn't get this one tbh
        RemoteData.Success result ->
            div []
                [ text "Success todo" ]


tmptmptmp title layerType layerData =
    case layerData of
        RemoteData.NotAsked ->
            button [ onClick <| GetLayerImage ] [ text <| "Import" ++ title ]

        RemoteData.Success layer ->
            div []
                [ text "Ok"
                , span [ onClick <| RemoveLayer layerType ] [ text "[Remove]" ]
                ]

        RemoteData.Failure error ->
            text "Error"

        RemoteData.Loading ->
            text "Loading.."


type alias MousePosition =
    { timeStamp : Float
    , offsetX : Float
    , offsetY : Float
    , button : Int
    }


decodeMousePosition : Decoder MousePosition
decodeMousePosition =
    Decode.map4 MousePosition
        (Decode.field "timeStamp" Decode.float)
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        (Decode.field "button" Decode.int)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ mouseDrag MouseMove
        , wheel MouseWheel
        , resize Resize
        , imageInformation GotImageInformation
        , onKeyDown (keyCode |> Decode.map KeyDown)
        , onMouseUp (Decode.map MouseUp decodeMousePosition)
        ]
