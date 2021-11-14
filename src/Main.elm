port module Main exposing (..)

import Base64
import Browser exposing (Document)
import Browser.Events exposing (onKeyDown, onKeyUp, onMouseUp)
import Bytes exposing (Bytes)
import Common exposing (Point, chainUpdate)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Attribute, Html, button, div, span, text)
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
    , timeline : WorkspaceTimeline
    , zPressed : Bool
    , xPressed : Bool
    , vPressed : Bool
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


{-| Timeline for implementing redo/undo.
TODO: Would be neat to have decent branching here?
-}
type alias WorkspaceTimeline =
    { current : Workspace.Model
    , past : List Workspace.Model
    , future : List Workspace.Model
    }


defaultWorkspaceTimeline : WorkspaceTimeline
defaultWorkspaceTimeline =
    { current = Workspace.defaultModel
    , past = []
    , future = []
    }


addTimelineEntry : WorkspaceTimeline -> Workspace.Model -> WorkspaceTimeline
addTimelineEntry timeline next =
    case timeline.future of
        [] ->
            { timeline
                | current = next
                , past = timeline.past ++ [ timeline.current ]
            }

        _ ->
            -- todo branching?
            { current = next
            , past = timeline.past ++ [ timeline.current ]
            , future = []
            }


undo : Model -> ( Model, Cmd Msg )
undo model =
    let
        timeline =
            model.timeline
    in
    case List.reverse timeline.past of
        [] ->
            ( model, Cmd.none )

        previous :: older ->
            ( { model
                | timeline =
                    { timeline
                        | current = previous
                        , past = List.reverse older
                        , future = timeline.current :: timeline.future
                    }
              }
            , Cmd.none
            )


redo : Model -> ( Model, Cmd Msg )
redo model =
    let
        timeline =
            model.timeline
    in
    case timeline.future of
        [] ->
            ( model, Cmd.none )

        next :: newer ->
            ( { model
                | timeline =
                    { timeline
                        | current = next
                        , past = timeline.past ++ [ timeline.current ]
                        , future = newer
                    }
              }
            , Cmd.none
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { layers = Dict.empty
      , dragging = False
      , shift = False
      , ctrl = False
      , lastMousePosition = MousePosition 0 0 0 0
      , canvasBoundingClientRect = BoundingClientRect 0 0 0 0 0
      , timeline = defaultWorkspaceTimeline
      , zPressed = False
      , xPressed = False
      , vPressed = False
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
    | Undo
    | Redo


type alias FileInfo =
    { name : String
    , mime : String
    , data : Bytes
    }


fromWorkspaceUpdate : ( Workspace.Model, Cmd Workspace.Msg, Bool ) -> Model -> ( Model, Cmd Msg )
fromWorkspaceUpdate ( wsModel, wsCmd, updateTimeline ) model =
    if updateTimeline then
        ( { model | timeline = addTimelineEntry model.timeline wsModel }, Cmd.map (\wsMsg -> Workspace wsMsg) wsCmd )

    else
        let
            timeline =
                model.timeline
        in
        ( { model | timeline = { timeline | current = wsModel } }, Cmd.map (\wsMsg -> Workspace wsMsg) wsCmd )


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
                    fromWorkspaceUpdate (Workspace.update (Workspace.AddLayer layerId) model.timeline.current) model
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
                                    [ setLayers ( List.filterMap (toExternalLayer m) m.timeline.current.layers, True )
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
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTransform transform) model.timeline.current) model

                _ ->
                    ( model, Cmd.none )

        RemoveLayer layerId ->
            -- todo needs refactoring
            let
                workspace =
                    model.timeline.current
            in
            ( { model
                | layers = Dict.remove layerId model.layers
                , timeline =
                    addTimelineEntry model.timeline
                        { workspace
                            | layers = List.filter (\l -> not <| l.id == layerId) workspace.layers
                        }
              }
            , Cmd.none
            )
                |> chainUpdate (\m -> ( m, setLayers ( List.filterMap (toExternalLayer m) m.timeline.current.layers, False ) ))

        --( { model | layers = List.take index model.layers ++ List.drop (index + 1) model.layers }, Cmd.none )
        MouseDown mousePosition ->
            -- Middle mouse button starts drag
            let
                cursor =
                    mousePositionToPoint model.canvasBoundingClientRect model.timeline.current.transform mousePosition
            in
            case mousePosition.button of
                1 ->
                    ( { model | dragging = True }, Cmd.none )

                0 ->
                    fromWorkspaceUpdate (Workspace.update (Workspace.LeftClick cursor) model.timeline.current) model

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
                    mousePositionToPoint model.canvasBoundingClientRect model.timeline.current.transform mousePosition

                dx =
                    model.lastMousePosition.offsetX - mousePosition.offsetX

                dy =
                    model.lastMousePosition.offsetY - mousePosition.offsetY
            in
            fromWorkspaceUpdate (Workspace.update (Workspace.SetCursor cursor ( dx, dy ) model.dragging) model.timeline.current) model
                |> chainUpdate (\m -> ( { m | lastMousePosition = mousePosition }, Cmd.none ))

        MouseOver ->
            fromWorkspaceUpdate (Workspace.update Workspace.Focus model.timeline.current) model
                |> chainUpdate (\m -> ( m, startWheel () ))

        MouseOut ->
            fromWorkspaceUpdate (Workspace.update Workspace.Unfocus model.timeline.current) model
                |> chainUpdate (\m -> ( m, endWheel () ))

        MouseWheel delta ->
            fromWorkspaceUpdate (Workspace.update (Workspace.ZoomDelta delta model.shift) model.timeline.current) model

        Resize boundingClientRect ->
            -- TODO store this somewhere
            ( { model | canvasBoundingClientRect = boundingClientRect }, Cmd.none )

        KeyDown key ->
            case key.keyCode of
                86 ->
                    -- v
                    update (Workspace Workspace.CycleLayers) model
                        |> chainUpdate (\m -> ( { m | vPressed = True }, Cmd.none ))

                16 ->
                    -- shift
                    ( { model | shift = True }, Cmd.none )

                17 ->
                    -- ctrl
                    ( { model | ctrl = True }, Cmd.none )

                27 ->
                    -- esc
                    fromWorkspaceUpdate (Workspace.update Workspace.ResetTool model.timeline.current) model

                81 ->
                    -- q
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTool (Workspace.SelectTool Nothing)) model.timeline.current) model

                65 ->
                    -- a
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTool Workspace.CreateThroughPadTool) model.timeline.current) model

                83 ->
                    -- s
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTool Workspace.CreateSurfacePadTool) model.timeline.current) model

                68 ->
                    -- d
                    fromWorkspaceUpdate (Workspace.update (Workspace.SetTool (Workspace.CreateTraceTool [])) model.timeline.current) model

                90 ->
                    -- z
                    update Undo model
                        |> chainUpdate (\m -> ( { m | zPressed = True }, Cmd.none ))

                88 ->
                    -- x
                    update Redo model
                        |> chainUpdate (\m -> ( { m | xPressed = True }, Cmd.none ))

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

                86 ->
                    -- v
                    ( { model | vPressed = False }, Cmd.none )

                90 ->
                    -- z
                    ( { model | zPressed = False }, Cmd.none )

                88 ->
                    -- x
                    ( { model | xPressed = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Workspace wsMsg ->
            fromWorkspaceUpdate (Workspace.update wsMsg model.timeline.current) model
                |> (case wsMsg of
                        Workspace.CycleLayers ->
                            chainUpdate (\m -> ( m, setLayers ( List.filterMap (toExternalLayer m) m.timeline.current.layers, False ) ))

                        _ ->
                            identity
                   )

        Undo ->
            undo model
                |> chainUpdate (\m -> ( m, setLayers ( List.filterMap (toExternalLayer m) m.timeline.current.layers, False ) ))

        Redo ->
            redo model
                |> chainUpdate (\m -> ( m, setLayers ( List.filterMap (toExternalLayer m) m.timeline.current.layers, False ) ))



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
                                , SvgA.viewBox <| transformToViewBox model.canvasBoundingClientRect model.timeline.current.transform
                                , SvgA.preserveAspectRatio "slice"
                                ]
                                --[ Maybe.withDefault (text "") <| Maybe.map (viewLayer model) <| List.head model.timeline.current.layers
                                --]
                                ([ Svg.g [ SvgA.id "cirdis-layers-mountpoint" ] [] ]
                                    ++ viewWorkspace model
                                )
                            ]
                        , div [ id "right-menu" ]
                            [ viewLayerList model.timeline.current.layers model.layers
                            , viewLayerSelect
                            , div [ id "key-row-0" ]
                                [ button [] [ text "", span [] [ text "1" ] ]
                                , button [] [ text "", span [] [ text "2" ] ]
                                , button [] [ text "", span [] [ text "3" ] ]
                                , button [] [ text "", span [] [ text "4" ] ]
                                ]
                            , div [ id "key-row-1" ]
                                [ button
                                    [ activeClass <| activeTool model (Workspace.SelectTool Nothing)
                                    , onClick <| Workspace <| Workspace.SetTool <| Workspace.SelectTool Nothing
                                    ]
                                    [ text "Select", span [] [ text "q" ] ]
                                , button [] [ text "", span [] [ text "w" ] ]
                                ]
                            , div [ id "key-row-2" ]
                                [ button
                                    [ activeClass <| activeTool model Workspace.CreateThroughPadTool
                                    , onClick <| Workspace <| Workspace.SetTool <| Workspace.CreateThroughPadTool
                                    ]
                                    [ text "THT", span [] [ text "a" ] ]
                                , button
                                    [ activeClass <| activeTool model Workspace.CreateSurfacePadTool
                                    , onClick <| Workspace <| Workspace.SetTool <| Workspace.CreateSurfacePadTool
                                    ]
                                    [ text "SMT", span [] [ text "s" ] ]
                                , button
                                    [ activeClass <| activeTool model (Workspace.CreateTraceTool [])
                                    , onClick <| Workspace <| Workspace.SetTool <| Workspace.CreateTraceTool []
                                    ]
                                    [ text "Trace", span [] [ text "d" ] ]
                                ]
                            , div [ id "key-row-3" ]
                                [ button [ activeClass model.zPressed, onClick Undo ] [ text "Undo", span [] [ text "z" ] ]
                                , button [ activeClass model.xPressed, onClick Redo ] [ text "Redo", span [] [ text "x" ] ]
                                , button [] [ text "", span [] [ text "c" ] ]
                                , button [ activeClass model.vPressed, onClick <| Workspace Workspace.CycleLayers ] [ text "Cycle", span [] [ text "v" ] ]
                                ]
                            , viewInfo model
                            ]
                        ]
                    ]
            )
            model
        ]
    }


activeClass : Bool -> Attribute Msg
activeClass isActive =
    if isActive then
        class "active"

    else
        class ""


activeTool : Model -> Workspace.Tool -> Bool
activeTool model tool =
    case ( model.timeline.current.tool, tool ) of
        ( Workspace.SelectTool _, Workspace.SelectTool _ ) ->
            True

        ( Workspace.CreateSurfacePadTool, Workspace.CreateSurfacePadTool ) ->
            True

        ( Workspace.CreateThroughPadTool, Workspace.CreateThroughPadTool ) ->
            True

        ( Workspace.CreateTraceTool _, Workspace.CreateTraceTool _ ) ->
            True

        ( Workspace.CreateZoneTool, Workspace.CreateZoneTool ) ->
            True

        _ ->
            False


viewWorkspace : Model -> List (Svg Msg)
viewWorkspace model =
    if List.isEmpty model.timeline.current.layers then
        []

    else
        [ fromWorkspaceSvg (Workspace.viewTool model.timeline.current)
        ]
            ++ List.map fromWorkspaceSvg (Workspace.viewMaybeLayerSurfaceConductors model.timeline.current.highlightNets (List.head model.timeline.current.layers))
            ++ List.map fromWorkspaceSvg (List.map (Workspace.viewThroughConductor model.timeline.current.highlightNets) model.timeline.current.conductors)


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
    if List.isEmpty wsLayers then
        div [ class "layer-list" ] [ text "No layers" ]

    else
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
    div [ id "import-layer" ]
        [ --input [ placeholder "New Layer", value title ] []
          button [ onClick <| GetLayerImage ] [ text <| "Import layer" ]
        ]


viewInfo : Model -> Html Msg
viewInfo model =
    let
        content =
            if List.isEmpty model.timeline.current.layers then
                text "Start by importing a new layer!"

            else
                case model.timeline.current.tool of
                    Workspace.SelectTool maybeConductor ->
                        text "Info about selection"

                    Workspace.CreateTraceTool constructionPoints ->
                        text <| "Trace thickness: " ++ String.fromFloat model.timeline.current.thickness

                    Workspace.CreateSurfacePadTool ->
                        text <| "Pad size: " ++ (String.fromFloat <| model.timeline.current.radius * 2)

                    Workspace.CreateThroughPadTool ->
                        text <| "Pad radius:: " ++ String.fromFloat model.timeline.current.radius

                    Workspace.CreateZoneTool ->
                        text "Zone tool"
    in
    div [] [ content ]



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
