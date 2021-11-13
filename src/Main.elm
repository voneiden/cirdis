port module Main exposing (..)

import Base64
import Browser exposing (Document)
import Browser.Events exposing (onKeyDown, onKeyUp, onMouseUp)
import Bytes exposing (Bytes)
import Common exposing (Point, chainUpdate)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Svg.Lazy as Svg
import Task
import Workspace



-- Todo things
-- allow scaling line segment width
-- copy segment width to next segment (set it actually to model so it stays on?)
-- placing pins and components?
-- staging area
-- via?
-- copper pours
-- undo/redo
-- snapping can be optimized with some kind of dict mapping to pixel regions
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
    { layers : Dict Int LayerData
    , dragging : Bool
    , shift : Bool
    , ctrl : Bool
    , lastMousePosition : MousePosition
    , canvasBoundingClientRect : BoundingClientRect
    , workspace : Workspace.Model
    }


type alias LayerData =
    { title : String
    , mimeType : String
    , b64Data : String
    }


type alias ExternalLayer =
    { id : Int
    , b64Data : String
    , mimeType : String
    , opacity : Int
    }


toExternalLayer : Model -> Workspace.Layer -> Maybe ExternalLayer
toExternalLayer model wsLayer =
    Maybe.map
        (\layerData ->
            { id = wsLayer.id
            , b64Data = layerData.b64Data
            , mimeType = layerData.mimeType
            , opacity = wsLayer.opacity
            }
        )
    <|
        Dict.get wsLayer.id model.layers


type alias MousePosition =
    { timeStamp : Float
    , offsetX : Float
    , offsetY : Float
    , button : Int
    }


defaultMousePosition : MousePosition
defaultMousePosition =
    { timeStamp = 0, offsetX = 0, offsetY = 0, button = 0 }


mousePositionToPoint : BoundingClientRect -> Workspace.Transform -> MousePosition -> Point
mousePositionToPoint b t mousePosition =
    let
        deltaX =
            mousePosition.offsetX - b.width / 2

        deltaY =
            mousePosition.offsetY - b.height / 2
    in
    { x = t.x + deltaX * t.z, y = t.y + deltaY * t.z }


type alias BoundingClientRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , top : Float
    }


defaultBoundingClientRect : BoundingClientRect
defaultBoundingClientRect =
    { x = 0, y = 0, width = 500, height = 500, top = 0 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { layers = Dict.empty
      , dragging = False
      , shift = False
      , ctrl = False
      , lastMousePosition = MousePosition 0 0 0 0
      , canvasBoundingClientRect = BoundingClientRect 0 0 0 0 0
      , workspace = Workspace.defaultModel
      }
    , canvasSize ()
    )



-- UPDATE


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
    | KeyDown Key
    | KeyUp Key
    | Workspace Workspace.Msg


type alias FileInfo =
    { name : String
    , mime : String
    , data : Bytes
    }


fromWorkspaceUpdate : ( Workspace.Model, Cmd Workspace.Msg ) -> Model -> ( Model, Cmd Msg )
fromWorkspaceUpdate ( wsModel, wsCmd ) model =
    ( { model | workspace = wsModel }, Cmd.map (\wsMsg -> Workspace wsMsg) wsCmd )


fromWorkspaceView : Html Workspace.Msg -> Html Msg
fromWorkspaceView html =
    Html.map (\msg -> Workspace msg) html


fromWorkspaceSvg : Svg Workspace.Msg -> Svg Msg
fromWorkspaceSvg svg =
    Svg.map (\msg -> Workspace msg) svg


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
                        layerId =
                            (+) 1 <| Maybe.withDefault 0 <| List.maximum <| Dict.keys model.layers

                        layerDefinition : LayerData
                        layerDefinition =
                            { title = fileInfo.name
                            , mimeType = fileInfo.mime
                            , b64Data = b64
                            }
                    in
                    fromWorkspaceUpdate (Workspace.update (Workspace.AddLayer layerId) model.workspace) model
                        |> chainUpdate
                            (\m ->
                                let
                                    layers =
                                        Dict.insert layerId layerDefinition model.layers
                                in
                                ( { m | layers = layers }
                                , Cmd.batch
                                    [ canvasSize ()
                                    ]
                                )
                            )
                        |> chainUpdate
                            (\m ->
                                ( m
                                , Cmd.batch
                                    [ setLayers ( List.filterMap (toExternalLayer m) m.workspace.layers, True )
                                    ]
                                )
                            )

                Nothing ->
                    ( model, Cmd.none )

        GotImageInformation imageInformations ->
            case List.take 1 imageInformations of
                info :: _ ->
                    let
                        -- Frame the layer nicely
                        widthRatio =
                            info.width / model.canvasBoundingClientRect.width

                        heightRatio =
                            info.height / model.canvasBoundingClientRect.height

                        transform =
                            { x = info.width / 2
                            , y = info.height / 2
                            , z = max widthRatio heightRatio + 0.1
                            }
                    in
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTransform transform) model.workspace) model

                _ ->
                    ( model, Cmd.none )

        RemoveLayer layerId ->
            -- todo needs refactoring
            let
                workspace =
                    model.workspace
            in
            ( { model
                | layers = Dict.remove layerId model.layers
                , workspace = { workspace | layers = List.filter (\l -> not <| l.id == layerId) workspace.layers }
              }
            , Cmd.none
            )
                |> chainUpdate (\m -> ( m, setLayers ( List.filterMap (toExternalLayer m) m.workspace.layers, False ) ))

        --( { model | layers = List.take index model.layers ++ List.drop (index + 1) model.layers }, Cmd.none )
        MouseDown mousePosition ->
            -- Middle mouse button starts drag
            let
                cursor =
                    mousePositionToPoint model.canvasBoundingClientRect model.workspace.transform mousePosition
            in
            case mousePosition.button of
                1 ->
                    ( { model | dragging = True }, Cmd.none )

                0 ->
                    fromWorkspaceUpdate (Workspace.update (Workspace.LeftClick cursor) model.workspace) model

                _ ->
                    ( model, Cmd.none )

        MouseUp mousePosition ->
            case mousePosition.button of
                1 ->
                    ( { model | dragging = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseMove mousePosition ->
            let
                cursor =
                    mousePositionToPoint model.canvasBoundingClientRect model.workspace.transform mousePosition

                dx =
                    model.lastMousePosition.offsetX - mousePosition.offsetX

                dy =
                    model.lastMousePosition.offsetY - mousePosition.offsetY
            in
            fromWorkspaceUpdate (Workspace.update (Workspace.SetCursor cursor ( dx, dy ) model.dragging) model.workspace) model
                |> chainUpdate (\m -> ( { m | lastMousePosition = mousePosition }, Cmd.none ))

        MouseOver ->
            fromWorkspaceUpdate (Workspace.update Workspace.Focus model.workspace) model
                |> chainUpdate (\m -> ( m, startWheel () ))

        MouseOut ->
            fromWorkspaceUpdate (Workspace.update Workspace.Unfocus model.workspace) model
                |> chainUpdate (\m -> ( m, endWheel () ))

        MouseWheel delta ->
            fromWorkspaceUpdate (Workspace.update (Workspace.ZoomDelta delta model.shift) model.workspace) model

        Resize boundingClientRect ->
            -- TODO store this somewhere
            ( { model | canvasBoundingClientRect = boundingClientRect }, Cmd.none )

        KeyDown key ->
            case key.keyCode of
                86 ->
                    -- v
                    fromWorkspaceUpdate (Workspace.update Workspace.CycleLayers model.workspace) model
                        |> chainUpdate (\m -> ( m, setLayers ( List.filterMap (toExternalLayer m) m.workspace.layers, False ) ))

                16 ->
                    -- shift
                    ( { model | shift = True }, Cmd.none )

                17 ->
                    -- ctrl
                    ( { model | ctrl = True }, Cmd.none )

                27 ->
                    -- esc
                    fromWorkspaceUpdate (Workspace.update Workspace.ResetTool model.workspace) model

                81 ->
                    -- q
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTool (Workspace.SelectTool Nothing)) model.workspace) model

                65 ->
                    -- a
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTool Workspace.CreateThroughPadTool) model.workspace) model

                83 ->
                    -- s
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTool Workspace.CreateSurfacePadTool) model.workspace) model

                68 ->
                    -- d
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTool (Workspace.CreateTraceTool [])) model.workspace) model

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            case key.keyCode of
                16 ->
                    -- shift
                    ( { model | shift = False }, Cmd.none )

                17 ->
                    -- ctrl
                    ( { model | ctrl = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Workspace wsMsg ->
            fromWorkspaceUpdate (Workspace.update wsMsg model.workspace) model



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
                        [ div
                            [ id "canvas-container"
                            , onMouseEnter MouseOver
                            , onMouseLeave MouseOut
                            ]
                            [ Svg.svg
                                [ SvgA.id "canvas"
                                , SvgE.preventDefaultOn "mousedown" (Decode.map (\msg -> ( msg, True )) (Decode.map MouseDown decodeMousePosition))
                                , SvgA.viewBox <| transformToViewBox model.canvasBoundingClientRect model.workspace.transform
                                , SvgA.preserveAspectRatio "slice"
                                ]
                                --[ Maybe.withDefault (text "") <| Maybe.map (viewLayer model) <| List.head model.workspace.layers
                                --]
                                ([ Svg.g [ SvgA.id "cirdis-layers-mountpoint" ] [] ]
                                    ++ [ fromWorkspaceSvg (Workspace.viewTool model.workspace)
                                       ]
                                    ++ List.map fromWorkspaceSvg (Workspace.viewMaybeLayerSurfaceConductors model.workspace.highlightNets (List.head model.workspace.layers))
                                    ++ List.map fromWorkspaceSvg (List.map (Workspace.viewThroughConductor model.workspace.highlightNets) model.workspace.conductors)
                                )
                            ]
                        , div [ id "right-menu" ]
                            [ viewLayerList model.workspace.layers model.layers
                            , viewLayerSelect
                            , div [] [ button [ onClick <| Workspace <| Workspace.SetTool <| Workspace.SelectTool Nothing ] [ text "Select" ] ]
                            , div [] [ button [ onClick <| Workspace <| Workspace.SetTool <| Workspace.CreateThroughPadTool ] [ text "Create THT Pad/Via" ] ]
                            , div [] [ button [ onClick <| Workspace <| Workspace.SetTool <| Workspace.CreateSurfacePadTool ] [ text "Create SMT Pad" ] ]
                            , div [] [ button [ onClick <| Workspace <| Workspace.SetTool <| Workspace.CreateTraceTool [] ] [ text "Create Trace" ] ]
                            ]
                        ]
                    ]
            )
            model
        ]
    }


{-| Convert a Transform into a SVG viewBox attribute value
-}
transformToViewBox : BoundingClientRect -> Workspace.Transform -> String
transformToViewBox r t =
    let
        width =
            r.width * t.z

        height =
            r.height * t.z
    in
    String.join " " [ String.fromFloat <| t.x - (width / 2), String.fromFloat <| t.y - (height / 2), String.fromFloat <| width, String.fromFloat <| height ]


{-| Render a layer using image data from the main model and active layer information from the workspace
-}
viewLayer : Model -> Workspace.Layer -> Svg Msg
viewLayer model layer =
    if layer.opacity == 0 then
        Svg.text ""

    else
        let
            maybeLayerData =
                Dict.get layer.id model.layers
        in
        case maybeLayerData of
            Just layerData ->
                Svg.lazy
                    (\ld ->
                        Svg.image
                            [ SvgA.class <| "layer-" ++ ld.title
                            , SvgA.xlinkHref <| "data:" ++ ld.mimeType ++ ";base64," ++ ld.b64Data
                            ]
                            []
                    )
                    layerData

            Nothing ->
                -- TODO some error or ability to reload new image?
                text ""


viewLayerList : List Workspace.Layer -> Dict Int LayerData -> Html Msg
viewLayerList wsLayers layers =
    div [ class "layer-list" ] <|
        List.filterMap
            (\wsLayer ->
                Dict.get wsLayer.id layers
                    |> Maybe.map (\layer -> viewLayerControls wsLayer layer)
            )
            wsLayers


viewLayerControls : Workspace.Layer -> LayerData -> Html Msg
viewLayerControls wsLayer layer =
    div [ class "layer-info" ]
        [ span [] [ text layer.title ]
        , span [ onClick <| RemoveLayer wsLayer.id ] [ text "[Remove]" ]
        ]


viewLayerSelect : Html Msg
viewLayerSelect =
    div []
        [ --input [ placeholder "New Layer", value title ] []
          button [ onClick <| GetLayerImage ] [ text <| "Import layer" ]
        ]



-- DECODERS


decodeMousePosition : Decoder MousePosition
decodeMousePosition =
    Decode.map4 MousePosition
        (Decode.field "timeStamp" Decode.float)
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        (Decode.field "button" Decode.int)


type alias Key =
    { keyCode : Int
    , shift : Bool
    , ctrl : Bool
    }


decodeKey : Decoder Key
decodeKey =
    Decode.map3 Key
        (Decode.field "keyCode" Decode.int)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ mouseDrag MouseMove
        , wheel MouseWheel
        , resize Resize
        , imageInformation GotImageInformation
        , onKeyDown (decodeKey |> Decode.map KeyDown)
        , onKeyUp (decodeKey |> Decode.map KeyUp)
        , onMouseUp (Decode.map MouseUp decodeMousePosition)
        ]



-- PORTS


port mouseDrag : (MousePosition -> msg) -> Sub msg


port startDrag : () -> Cmd msg


port wheel : (Float -> msg) -> Sub msg


port startWheel : () -> Cmd msg


port endWheel : () -> Cmd msg


port resize : (BoundingClientRect -> msg) -> Sub msg


port canvasSize : () -> Cmd msg


port checkImages : () -> Cmd msg


port imageInformation : (List ImageInformation -> msg) -> Sub msg


port setLayers : ( List ExternalLayer, Bool ) -> Cmd msg


type alias ImageInformation =
    { layer : String
    , height : Float
    , width : Float
    }
