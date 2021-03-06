port module Main exposing (..)

import Base64
import Browser exposing (Document)
import Browser.Events exposing (onKeyUp, onMouseUp)
import Bytes exposing (Bytes)
import Common exposing (Point, ThreePoints(..), TwoPoints(..), chainUpdate)
import Conductor
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select as Select
import Form
import Html exposing (Attribute, Html, button, div, h5, input, p, span, text)
import Html.Attributes exposing (class, disabled, id, placeholder, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy exposing (lazy)
import Io exposing (BoundingClientRect, LayerData, MainModel, MousePosition, WorkspaceTimeline, decodeMainModel, defaultWorkspaceTimeline, encodeMainModel, encodedSvgModel)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Svg.Lazy as Svg
import Task
import Tool
import Vector
import VirtualDom
import Visual exposing (viewDimensions)
import Workspace


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    MainModel


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


mousePositionToPoint : BoundingClientRect -> Workspace.Transform -> MousePosition -> Point
mousePositionToPoint b t mousePosition =
    let
        deltaX =
            mousePosition.offsetX - b.width / 2

        deltaY =
            mousePosition.offsetY - b.height / 2
    in
    { x = t.x + deltaX * t.z, y = t.y + deltaY * t.z }


{-| Timeline for implementing redo/undo.
TODO: Would be neat to have decent branching here?
-}
addTimelineEntry : Workspace.Model -> Model -> Model
addTimelineEntry next model =
    let
        timeline =
            model.timeline
    in
    { model
        | timeline =
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { layers = Dict.empty
      , dragging = False
      , shift = False
      , ctrl = False
      , lastMousePosition = MousePosition 0 0 0 0
      , canvasBoundingClientRect = BoundingClientRect 0 0 0 0 0
      , timeline = defaultWorkspaceTimeline
      , keyDownPreventDefault = True
      , ePressed = False
      , zPressed = False
      , xPressed = False
      , vPressed = False
      , includeSource = False
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
    | Erase
    | Undo
    | Redo
    | StartCapture
    | StopCapture
    | FormWelcomeImportLayer
    | FormWelcomeImportProject
    | FormRefApply Form.RefFormData
    | FormRefClear
    | FormNetApply Form.NetFormData
    | SaveProject
    | DownloadProject String
    | DownloadProjectFailure ()
    | GetLoadProject
    | GotLoadProject File
    | GotLoadProjectData String
    | LoadProjectSuccess ProjectData
    | LoadProjectFailure ()


type alias FileInfo =
    { name : String
    , mime : String
    , data : Bytes
    }


fromWorkspaceUpdate : ( Workspace.Model, Cmd Workspace.Msg, Bool ) -> Model -> ( Model, Cmd Msg )
fromWorkspaceUpdate ( wsModel, wsCmd, updateTimeline ) model =
    if updateTimeline then
        ( addTimelineEntry wsModel model, Cmd.map (\wsMsg -> Workspace wsMsg) wsCmd )

    else
        ( replaceCurrentWorkspace wsModel model, Cmd.map (\wsMsg -> Workspace wsMsg) wsCmd )


replaceCurrentWorkspace : Workspace.Model -> Model -> Model
replaceCurrentWorkspace wsModel model =
    let
        timeline =
            model.timeline
    in
    { model | timeline = { timeline | current = wsModel } }


fromVisualSvg : Svg Visual.Msg -> Svg Msg
fromVisualSvg svg =
    Svg.map (\msg -> Workspace <| Workspace.VisualElementMsg <| msg) svg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( updatedModel, cmd ) =
            doUpdate msg model
    in
    -- stop keydown
    case ( updatedModel.keyDownPreventDefault, updatedModel.timeline.current.form ) of
        ( False, Form.NoForm ) ->
            ( { updatedModel | keyDownPreventDefault = True }, Cmd.batch [ cmd, keyDownPreventDefault () ] )

        ( True, Form.NoForm ) ->
            ( updatedModel, cmd )

        ( True, _ ) ->
            ( { updatedModel | keyDownPreventDefault = False }, Cmd.batch [ cmd, keyDownAllowDefault () ] )

        ( False, _ ) ->
            ( updatedModel, cmd )


doUpdate : Msg -> Model -> ( Model, Cmd Msg )
doUpdate msg model =
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
                            { id = layerId
                            , title = fileInfo.name
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
              }
                |> addTimelineEntry
                    { workspace
                        | layers = List.filter (\l -> not <| l.id == layerId) workspace.layers
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
            fromWorkspaceUpdate (Workspace.update (Workspace.ZoomDelta delta model.shift model.ctrl) model.timeline.current) model

        Resize boundingClientRect ->
            -- TODO store this somewhere
            ( { model | canvasBoundingClientRect = boundingClientRect }, Cmd.none )

        KeyDown key ->
            if model.keyDownPreventDefault then
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
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.Reset) model.timeline.current) model

                    81 ->
                        -- q
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.SetTool <| Tool.SelectTool Conductor.NoInteraction Conductor.NoInteraction) model.timeline.current) model

                    87 ->
                        -- w
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.SetTool <| Tool.CreateDistanceDimension Nothing) model.timeline.current) model

                    65 ->
                        -- a
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.SetTool <| Tool.CreateThroughPadTool) model.timeline.current) model

                    83 ->
                        -- s
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.SetTool <| Tool.CreateSurfacePadTool) model.timeline.current) model

                    68 ->
                        -- d
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.SetTool <| Tool.CreateTraceTool [] Conductor.NoInteraction) model.timeline.current) model

                    -- Partiboi
                    69 ->
                        -- e
                        update Erase model
                            |> chainUpdate (\m -> ( { m | ePressed = True }, Cmd.none ))

                    90 ->
                        -- z
                        update Undo model
                            |> chainUpdate (\m -> ( { m | zPressed = True }, Cmd.none ))

                    88 ->
                        -- x
                        update Redo model
                            |> chainUpdate (\m -> ( { m | xPressed = True }, Cmd.none ))

                    49 ->
                        -- 1
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.SetSubTool 1) model.timeline.current) model

                    50 ->
                        -- 2
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.SetSubTool 2) model.timeline.current) model

                    51 ->
                        -- 3
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.SetSubTool 3) model.timeline.current) model

                    52 ->
                        -- 4
                        fromWorkspaceUpdate (Workspace.update (Workspace.ToolMsg <| Tool.SetSubTool 4) model.timeline.current) model

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        KeyUp key ->
            case key.keyCode of
                16 ->
                    -- shift
                    ( { model | shift = False }, Cmd.none )

                17 ->
                    -- ctrl
                    ( { model | ctrl = False }, Cmd.none )

                -- Partiboi
                69 ->
                    -- e
                    ( { model | ePressed = False }, Cmd.none )

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

        Erase ->
            update (Workspace <| Workspace.ToolMsg Tool.Erase) model

        Undo ->
            undo model
                |> chainUpdate (\m -> ( m, setLayers ( List.filterMap (toExternalLayer m) m.timeline.current.layers, False ) ))

        Redo ->
            redo model
                |> chainUpdate (\m -> ( m, setLayers ( List.filterMap (toExternalLayer m) m.timeline.current.layers, False ) ))

        StartCapture ->
            ( { model | keyDownPreventDefault = True }, keyDownPreventDefault () )

        StopCapture ->
            ( { model | keyDownPreventDefault = False }, keyDownAllowDefault () )

        FormWelcomeImportLayer ->
            let
                workspace =
                    model.timeline.current

                newWorkspace =
                    { workspace | form = Form.NoForm }
            in
            doUpdate GetLayerImage (replaceCurrentWorkspace newWorkspace model)

        FormWelcomeImportProject ->
            let
                workspace =
                    model.timeline.current
            in
            doUpdate GetLoadProject (addTimelineEntry { workspace | form = Form.NoForm } model)

        FormRefApply refForm ->
            case String.toFloat refForm.inputDistance of
                Just distance ->
                    let
                        workspace =
                            model.timeline.current

                        unit =
                            if refForm.inputUnit == "" then
                                "mm"

                            else
                                refForm.inputUnit
                    in
                    ( addTimelineEntry
                        { workspace
                            | form = Form.NoForm
                            , ref = Just { p1 = refForm.p1, p2 = refForm.p2, value = distance, unit = unit, ratio = distance / Vector.len (Vector.sub refForm.p1 refForm.p2) }
                            , tool = Tool.CreateDistanceDimension Nothing
                        }
                        model
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        FormRefClear ->
            let
                workspace =
                    model.timeline.current
            in
            ( addTimelineEntry
                { workspace
                    | form = Form.NoForm
                    , ref = Nothing
                    , tool = Tool.DefineReferenceFrame Nothing Nothing
                }
                model
            , Cmd.none
            )

        SaveProject ->
            ( { model | includeSource = True }, saveProject () )

        GetLoadProject ->
            ( model
            , Select.file [ "image/svg+xml" ] GotLoadProject
            )

        DownloadProject string ->
            ( { model | includeSource = False }, File.Download.string "cirdis-project.svg" "image/svg+xml" string )

        GotLoadProject file ->
            ( model, Task.perform GotLoadProjectData <| File.toString file )

        GotLoadProjectData data ->
            ( model, loadSvg data )

        LoadProjectSuccess projectData ->
            case Decode.decodeString (decodeMainModel model projectData.layerData) projectData.data of
                Ok newModel ->
                    ( newModel, setLayers ( List.filterMap (toExternalLayer newModel) newModel.timeline.current.layers, True ) )

                Err error ->
                    -- TODO show error form
                    ( model, Cmd.none )

        LoadProjectFailure () ->
            -- TODO show error form
            ( model, Cmd.none )

        DownloadProjectFailure () ->
            -- TODO show error form
            ( model, Cmd.none )

        FormNetApply netFormData ->
            -- TODO
            ( model, Cmd.none )


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



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Circuit Dissector"
    , body =
        [ lazy
            (\_ ->
                div
                    [ id "root"
                    ]
                    [ div [ class "flex-row" ]
                        [ sidebar model
                        , div
                            [ id "canvas-container"
                            , onMouseEnter MouseOver
                            , onMouseLeave MouseOut
                            ]
                            [ Svg.svg
                                [ SvgA.id "canvas"
                                , SvgE.preventDefaultOn "mousedown" (Decode.map (\msg -> ( msg, True )) (Decode.map MouseDown decodeMousePosition))
                                , SvgA.viewBox <| transformToViewBox model.canvasBoundingClientRect model.timeline.current.transform

                                --, SvgA.preserveAspectRatio "slice"
                                , SvgA.class <| "tool-" ++ Tool.toolToString model.timeline.current.tool
                                , VirtualDom.attribute "xmlns" "http://www.w3.org/2000/svg"
                                , VirtualDom.attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
                                , SvgE.onClick (Workspace <| Workspace.VisualElementMsg <| Visual.Click Visual.Background)
                                , SvgE.onMouseOver (Workspace <| Workspace.VisualElementMsg <| Visual.MouseOver Visual.Background)

                                --
                                ]
                                --[ Maybe.withDefault (text "") <| Maybe.map (viewLayer model) <| List.head model.timeline.current.layers
                                --]
                                ([ Svg.g [ SvgA.id "cirdis-layers-mountpoint" ] [] ]
                                    ++ viewWorkspace model
                                    ++ (if model.includeSource then
                                            [ encodedSvgModel model ]

                                        else
                                            []
                                       )
                                )
                            ]
                        ]
                    , viewForm model
                    ]
            )
            model
        ]
    }


sidebarKeyRow0 : Workspace.Model -> Html Msg
sidebarKeyRow0 wsModel =
    let
        hasRef =
            not (wsModel.ref == Nothing)

        tool =
            wsModel.tool
    in
    case tool of
        Tool.CreateSurfacePadTool ->
            surfacePadSubTools tool

        Tool.CreateNumberedSurfacePad _ ->
            surfacePadSubTools tool

        Tool.CreateSoicSurfacePad _ ->
            surfacePadSubTools tool

        Tool.CreateRowSurfacePad _ _ ->
            surfacePadSubTools tool

        Tool.CreateThroughPadTool ->
            throughPadSubTools tool

        Tool.CreateNumberedThroughPad _ ->
            throughPadSubTools tool

        Tool.CreateDipThroughPad _ ->
            throughPadSubTools tool

        Tool.CreateRowThroughPad _ _ ->
            throughPadSubTools tool

        Tool.DefineReferenceFrame _ _ ->
            dimensionSubTools hasRef tool

        Tool.CreateDistanceDimension _ ->
            dimensionSubTools hasRef tool

        Tool.CreateAngleDimension _ _ ->
            dimensionSubTools hasRef tool

        _ ->
            blankSubTools


toolAttrsAndText : ( Tool.Tool, Bool ) -> Int -> ( List (Attribute Msg), Html Msg )
toolAttrsAndText ( tool, active ) index =
    let
        activeAttr =
            if active then
                class "active"

            else
                class ""

        clickAttr =
            onClick <| Workspace <| Workspace.ToolMsg <| Tool.SetSubTool index
    in
    case tool of
        Tool.SelectTool _ _ ->
            ( [ activeAttr, clickAttr ], text "" )

        Tool.CreateTraceTool _ _ ->
            ( [ activeAttr, clickAttr ], text "" )

        Tool.CreateSurfacePadTool ->
            ( [ activeAttr, clickAttr ], span [ class "bordered" ] [ text "" ] )

        Tool.CreateNumberedSurfacePad pinNumber ->
            ( [ activeAttr, clickAttr ], span [ class "bordered" ] [ text <| String.fromInt pinNumber ] )

        Tool.CreateSoicSurfacePad _ ->
            ( [ activeAttr, clickAttr ], text "SOIC" )

        Tool.CreateRowSurfacePad _ _ ->
            ( [ activeAttr, clickAttr ], text "Row" )

        Tool.CreateThroughPadTool ->
            ( [ activeAttr, clickAttr ], span [ class "circled" ] [ text "" ] )

        Tool.CreateNumberedThroughPad pinNumber ->
            ( [ activeAttr, clickAttr ], span [ class "circled" ] [ text <| String.fromInt pinNumber ] )

        Tool.CreateDipThroughPad _ ->
            ( [ activeAttr, clickAttr ], text "DIP" )

        Tool.CreateRowThroughPad _ _ ->
            ( [ activeAttr, clickAttr ], text "Row" )

        Tool.CreateZoneTool ->
            ( [ activeAttr, clickAttr ], text "" )

        Tool.DefineReferenceFrame _ _ ->
            ( [ activeAttr, clickAttr ], text "Ref" )

        Tool.CreateDistanceDimension _ ->
            ( [ activeAttr, clickAttr ], text "Dist" )

        Tool.CreateAngleDimension _ _ ->
            ( [ activeAttr, clickAttr ], text "Angle" )


pickTool : Tool.Tool -> Tool.Tool -> ( Tool.Tool, Bool )
pickTool rowTool selectedTool =
    if Tool.resetTool rowTool == Tool.resetTool selectedTool then
        ( selectedTool, True )

    else
        ( rowTool, False )


surfacePadSubTools : Tool.Tool -> Html Msg
surfacePadSubTools tool =
    let
        ( b1Attrs, b1Text ) =
            toolAttrsAndText (pickTool Tool.CreateSurfacePadTool tool) 1

        ( b2Attrs, b2Text ) =
            toolAttrsAndText (pickTool (Tool.CreateNumberedSurfacePad 1) tool) 2

        ( b3Attrs, b3Text ) =
            toolAttrsAndText (pickTool (Tool.CreateSoicSurfacePad NoneOfThree) tool) 3

        ( b4Attrs, b4Text ) =
            toolAttrsAndText (pickTool (Tool.CreateRowSurfacePad 1 NoneOfTwo) tool) 4
    in
    div [ id "key-row-0" ]
        [ button b1Attrs [ b1Text, span [ class "keycode" ] [ text "1" ] ]
        , button b2Attrs [ b2Text, span [ class "keycode" ] [ text "2" ] ]
        , button b3Attrs [ b3Text, span [ class "keycode" ] [ text "3" ] ]
        , button b4Attrs [ b4Text, span [ class "keycode" ] [ text "4" ] ]
        ]


throughPadSubTools : Tool.Tool -> Html Msg
throughPadSubTools tool =
    let
        ( b1Attrs, b1Text ) =
            toolAttrsAndText (pickTool Tool.CreateThroughPadTool tool) 1

        ( b2Attrs, b2Text ) =
            toolAttrsAndText (pickTool (Tool.CreateNumberedThroughPad 1) tool) 2

        ( b3Attrs, b3Text ) =
            toolAttrsAndText (pickTool (Tool.CreateDipThroughPad NoneOfThree) tool) 3

        ( b4Attrs, b4Text ) =
            toolAttrsAndText (pickTool (Tool.CreateRowThroughPad 1 NoneOfTwo) tool) 4
    in
    div [ id "key-row-0" ]
        [ button b1Attrs [ b1Text, span [ class "keycode" ] [ text "1" ] ]
        , button b2Attrs [ b2Text, span [ class "keycode" ] [ text "2" ] ]
        , button b3Attrs [ b3Text, span [ class "keycode" ] [ text "3" ] ]
        , button b4Attrs [ b4Text, span [ class "keycode" ] [ text "4" ] ]
        ]


dimensionSubTools : Bool -> Tool.Tool -> Html Msg
dimensionSubTools hasRef tool =
    if hasRef then
        let
            ( b1Attrs, b1Text ) =
                toolAttrsAndText (pickTool (Tool.CreateDistanceDimension Nothing) tool) 1

            ( b2Attrs, b2Text ) =
                toolAttrsAndText (pickTool (Tool.CreateAngleDimension Nothing Nothing) tool) 2

            ( b3Attrs, b3Text ) =
                ( [ disabled True ], text "" )

            ( b4Attrs, b4Text ) =
                toolAttrsAndText (pickTool (Tool.DefineReferenceFrame Nothing Nothing) tool) 4
        in
        div [ id "key-row-0" ]
            [ button b1Attrs [ b1Text, span [ class "keycode" ] [ text "1" ] ]
            , button b2Attrs [ b2Text, span [ class "keycode" ] [ text "2" ] ]
            , button b3Attrs [ b3Text, span [ class "keycode" ] [ text "3" ] ]
            , button b4Attrs [ b4Text, span [ class "keycode" ] [ text "4" ] ]
            ]

    else
        blankSubTools


blankSubTools : Html Msg
blankSubTools =
    div [ id "key-row-0" ]
        [ button [] [ text "", span [ class "keycode" ] [ text "1" ] ]
        , button [] [ text "", span [ class "keycode" ] [ text "2" ] ]
        , button [] [ text "", span [ class "keycode" ] [ text "3" ] ]
        , button [] [ text "", span [ class "keycode" ] [ text "4" ] ]
        ]


workspaceMsg : Workspace.Msg -> Msg
workspaceMsg msg =
    Workspace <| msg


toolMsg : Tool.Msg -> Msg
toolMsg msg =
    Workspace <| Workspace.ToolMsg <| msg


sidebar : Model -> Html Msg
sidebar model =
    div [ id "sidebar" ]
        [ viewLayerList model.timeline.current.layers model.layers
        , viewLayerSelect
        , viewProjectActions
        , sidebarKeyRow0 model.timeline.current
        , div [ id "key-row-1" ]
            [ button
                [ activeClass <| activeTool model (Tool.SelectTool Conductor.NoInteraction Conductor.NoInteraction)
                , onClick <| toolMsg <| Tool.SetTool <| Tool.SelectTool Conductor.NoInteraction Conductor.NoInteraction
                ]
                [ text "Select", span [ class "keycode" ] [ text "q" ] ]
            , button
                [ activeClass <| activeTool model (Tool.DefineReferenceFrame Nothing Nothing)
                , onClick <| toolMsg <| Tool.SetTool <| Tool.CreateDistanceDimension Nothing
                ]
                [ text "Gauge", span [ class "keycode" ] [ text "w" ] ]
            , button
                [ activeClass model.ePressed
                , onClick Erase
                ]
                [ text "Erase", span [ class "keycode" ] [ text "e" ] ]
            ]
        , div [ id "key-row-2" ]
            [ button
                [ activeClass <| activeTool model Tool.CreateThroughPadTool
                , onClick <| toolMsg <| Tool.SetTool <| Tool.CreateThroughPadTool
                ]
                [ text "THT", span [ class "keycode" ] [ text "a" ] ]
            , button
                [ activeClass <| activeTool model Tool.CreateSurfacePadTool
                , onClick <| toolMsg <| Tool.SetTool <| Tool.CreateSurfacePadTool
                ]
                [ text "SMT", span [ class "keycode" ] [ text "s" ] ]
            , button
                [ activeClass <| activeTool model (Tool.CreateTraceTool [] Conductor.NoInteraction)
                , onClick <| toolMsg <| Tool.SetTool <| Tool.CreateTraceTool [] Conductor.NoInteraction
                ]
                [ text "Trace", span [ class "keycode" ] [ text "d" ] ]
            ]
        , div [ id "key-row-3" ]
            [ button [ activeClass model.zPressed, onClick Undo ] [ text "Undo", span [ class "keycode" ] [ text "z" ] ]
            , button [ activeClass model.xPressed, onClick Redo ] [ text "Redo", span [ class "keycode" ] [ text "x" ] ]
            , button [] [ text "", span [] [ text "c" ] ]
            , button [ activeClass model.vPressed, onClick <| Workspace Workspace.CycleLayers ] [ text "Cycle", span [ class "keycode" ] [ text "v" ] ]
            ]
        , viewInfo model
        ]


activeClass : Bool -> Attribute Msg
activeClass isActive =
    if isActive then
        class "active"

    else
        class ""


activeTool : Model -> Tool.Tool -> Bool
activeTool model tool =
    case ( model.timeline.current.tool, tool ) of
        ( Tool.SelectTool _ _, Tool.SelectTool _ _ ) ->
            True

        ( Tool.CreateSurfacePadTool, Tool.CreateSurfacePadTool ) ->
            True

        ( Tool.CreateNumberedSurfacePad _, Tool.CreateSurfacePadTool ) ->
            True

        ( Tool.CreateSoicSurfacePad _, Tool.CreateSurfacePadTool ) ->
            True

        ( Tool.CreateRowSurfacePad _ _, Tool.CreateSurfacePadTool ) ->
            True

        ( Tool.CreateThroughPadTool, Tool.CreateThroughPadTool ) ->
            True

        ( Tool.CreateNumberedThroughPad _, Tool.CreateThroughPadTool ) ->
            True

        ( Tool.CreateDipThroughPad _, Tool.CreateThroughPadTool ) ->
            True

        ( Tool.CreateRowThroughPad _ _, Tool.CreateThroughPadTool ) ->
            True

        ( Tool.CreateTraceTool _ _, Tool.CreateTraceTool _ _ ) ->
            True

        ( Tool.CreateZoneTool, Tool.CreateZoneTool ) ->
            True

        ( Tool.DefineReferenceFrame _ _, Tool.DefineReferenceFrame _ _ ) ->
            True

        ( Tool.CreateDistanceDimension _, Tool.DefineReferenceFrame _ _ ) ->
            True

        ( Tool.CreateAngleDimension _ _, Tool.DefineReferenceFrame _ _ ) ->
            True

        _ ->
            False


viewWorkspace : Model -> List (Svg Msg)
viewWorkspace model =
    let
        current =
            model.timeline.current

        conductors =
            current.conductors
    in
    if List.isEmpty model.timeline.current.layers then
        []

    else
        [ fromVisualSvg (Visual.viewTool model.timeline.current)
        ]
            ++ [ fromVisualSvg (Visual.viewSurfaceConductors model.timeline.current model.timeline.current.layers) ]
            ++ [ Svg.lazy2
                    (\_ _ ->
                        fromVisualSvg <| Visual.viewLazyThroughConductors model.timeline.current model.timeline.current.conductors
                    )
                    model
                    conductors
               ]
            ++ [ fromVisualSvg (viewDimensions current current.dimensions) ]


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
    div [ class "import-layer" ]
        [ --input [ placeholder "New Layer", value title ] []
          button [ onClick <| GetLayerImage ] [ text <| "Import layer" ]
        ]


viewProjectActions : Html Msg
viewProjectActions =
    div [ class "project-actions" ]
        [ button [ onClick <| SaveProject ] [ text <| "Save project" ]
        , button [ onClick <| GetLoadProject ] [ text <| "Load project" ]
        ]


viewInfo : Model -> Html Msg
viewInfo model =
    let
        content =
            if List.isEmpty model.timeline.current.layers then
                text "Start by importing a new layer!"

            else
                case model.timeline.current.tool of
                    Tool.SelectTool selection highlight ->
                        text "Info about selection"

                    Tool.CreateTraceTool constructionPoints highlight ->
                        text <| "Trace thickness: " ++ String.fromFloat model.timeline.current.thickness

                    Tool.CreateSurfacePadTool ->
                        text <| "Pad size: " ++ (String.fromFloat <| model.timeline.current.radius * 2)

                    Tool.CreateThroughPadTool ->
                        text <| "Pad radius:: " ++ String.fromFloat model.timeline.current.radius

                    Tool.CreateZoneTool ->
                        text "Zone tool"

                    Tool.CreateNumberedSurfacePad pinNumber ->
                        text <| "Place pin number " ++ String.fromInt pinNumber

                    Tool.CreateSoicSurfacePad threePoints ->
                        case threePoints of
                            NoneOfThree ->
                                text "Place pin 1"

                            OneOfThree _ ->
                                text "Place last pin of the row"

                            TwoOfThree _ _ ->
                                text "Move one pin towards pin 1"

                            ThreeOfThree _ _ _ ->
                                text "Place second row"

                    Tool.CreateRowSurfacePad _ twoPoints ->
                        case twoPoints of
                            NoneOfTwo ->
                                text "Place 1st pin"

                            OneOfTwo _ ->
                                text "Place last pin of the row"

                            TwoOfTwo _ _ ->
                                text "Move one pin towards pin 1"

                    Tool.CreateNumberedThroughPad pinNumber ->
                        text <| "Place pin " ++ String.fromInt pinNumber

                    Tool.CreateDipThroughPad threePoints ->
                        case threePoints of
                            NoneOfThree ->
                                text "Place pin 1"

                            OneOfThree _ ->
                                text "Place last pin of the row"

                            TwoOfThree _ _ ->
                                text "Move one pin towards pin 1"

                            ThreeOfThree _ _ _ ->
                                text "Place second row"

                    Tool.CreateRowThroughPad _ twoPoints ->
                        case twoPoints of
                            NoneOfTwo ->
                                text "Place 1st pin"

                            OneOfTwo _ ->
                                text "Place last pin of the row"

                            TwoOfTwo _ _ ->
                                text "Move one pin towards pin 1"

                    Tool.DefineReferenceFrame mp1 mp2 ->
                        let
                            current =
                                model.timeline.current

                            ref =
                                current.ref
                        in
                        case ( ref, mp1, mp2 ) of
                            ( Nothing, Nothing, Nothing ) ->
                                text "Place first reference point"

                            ( Nothing, Just _, Nothing ) ->
                                text "Place second reference point"

                            _ ->
                                div []
                                    [ text "TODO use C to clear"
                                    ]

                    Tool.CreateDistanceDimension mp1 ->
                        case mp1 of
                            Nothing ->
                                text "Calculate distance between two points. Place first measurement point"

                            Just _ ->
                                text "Place second measurement point"

                    Tool.CreateAngleDimension mp1 mp2 ->
                        case ( mp1, mp2 ) of
                            ( Nothing, Nothing ) ->
                                text "Place first radial point"

                            ( Just _, Nothing ) ->
                                text "Place center point"

                            _ ->
                                text "Place second radial point"
    in
    div [] [ content ]


viewForm : Model -> Html Msg
viewForm model =
    let
        viewData =
            { welcome =
                { importLayerMsg = FormWelcomeImportLayer
                , importSvgMsg = FormWelcomeImportProject -- TODO implement
                }
            , ref =
                { apply = FormRefApply
                , clear = FormRefClear
                }
            , net =
                { apply = FormNetApply
                , options = []
                }
            }

        form =
            model.timeline.current.form

        visible =
            if form == Form.NoForm then
                class ""

            else
                class "visible"
    in
    div [ id "form-root", visible ] [ Form.view viewData (Workspace << Workspace.FormMsg) form ]



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

        --, onKeyDown (decodeKey |> Decode.map KeyDown)
        , onKeyUp (decodeKey |> Decode.map KeyUp)
        , onMouseUp (Decode.map MouseUp decodeMousePosition)
        , keyDown KeyDown
        , downloadProject DownloadProject
        , loadProjectSuccess LoadProjectSuccess
        , loadProjectFailure LoadProjectFailure
        ]



-- PORTS


port keyDown : (Key -> msg) -> Sub msg


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


port keyDownPreventDefault : () -> Cmd msg


port keyDownAllowDefault : () -> Cmd msg


port saveProject : () -> Cmd msg


port downloadProject : (String -> msg) -> Sub msg


port downloadProjectFailure : (() -> msg) -> Sub msg


port loadSvg : String -> Cmd msg


type alias ProjectData =
    { data : String
    , layerData : List ( Int, String )
    }


port loadProjectSuccess : (ProjectData -> msg) -> Sub msg


port loadProjectFailure : (() -> msg) -> Sub msg


type alias ImageInformation =
    { layer : String
    , height : Float
    , width : Float
    }
