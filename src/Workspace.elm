module Workspace exposing (..)

import Common exposing (Color, CtrlPressed, Dragging, Point, Radius, ReferenceFrame, ShiftPressed, Thickness, Width, chainUpdate3, cycle)
import Conductor exposing (..)
import Form
import Tool exposing (Tool)
import Visual



-- MODEL


type alias Model =
    { layers : List Layer
    , cursor : Point
    , focused : Bool
    , transform : Transform
    , canvas : Canvas
    , radius : Float
    , thickness : Float
    , tool : Tool
    , conductors : List ThroughConductor
    , nextNetId : Int -- Running id for nets
    , snapDistance : Float
    , autoNetColor : String
    , highlightNets : List Net
    , select : List Visual.VisualElement
    , ref : Maybe ReferenceFrame
    , form : Form.Form
    }


defaultModel : Model
defaultModel =
    { layers = []
    , cursor = Point 0 0
    , focused = False
    , transform = defaultTransform
    , canvas = { width = 0, height = 0 }
    , radius = 10
    , thickness = 5
    , tool = Tool.SelectTool Nothing
    , conductors = []
    , nextNetId = 1
    , snapDistance = 10
    , autoNetColor = ""
    , highlightNets = []
    , select = []
    , ref = Nothing
    , form = Form.WelcomeForm
    }


type alias Layer =
    { id : Int
    , opacity : Int
    , conductors : List SurfaceConductor
    }


newLayer : Int -> Layer
newLayer layerId =
    { id = layerId
    , opacity = 100
    , conductors = []
    }


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
translateTransform : Transform -> Float -> Float -> Transform
translateTransform t x y =
    { t | x = t.x + x * t.z, y = t.y + y * t.z }


{-| Apply zoom to transform. While it accepts a float,
actually it only cares about the sign. This is due to
differences with browser wheel event delta's.
-}
zoomTransform : Transform -> Float -> Transform
zoomTransform t z =
    let
        -- 66.25 seems to be the default step for chrome on linux, so use that as a baseline
        multiplier =
            20 * (z / 66.25) / 100 + 1

        newZ =
            t.z * multiplier
    in
    { t | z = newZ }


type alias Canvas =
    { width : Float
    , height : Float
    }


type alias ColorDefinition =
    { stroke : String
    , fill : String
    }



-- UPDATE


type Msg
    = SetCursor Point ( Float, Float ) Dragging
    | LeftClick Point
    | ZoomDelta Float ShiftPressed CtrlPressed
    | SetTransform Transform
    | AddLayer Int
    | CycleLayers
    | Focus
    | Unfocus
    | VisualElementMsg Visual.Msg
    | ToolMsg Tool.Msg
    | FormMsg Form.Msg


update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update msg model =
    case msg of
        SetCursor point ( dx, dy ) dragging ->
            if dragging then
                ( { model | cursor = point, transform = translateTransform model.transform dx dy }, Cmd.none, False )

            else
                case model.tool of
                    Tool.CreateTraceTool cps ->
                        let
                            snapPoint =
                                snapTo model.snapDistance point model.conductors (activeLayerSurfaceConductors model) 0
                        in
                        ( { model | cursor = point }
                            |> createTraceToHighlightNets (snapPoint :: cps)
                        , Cmd.none
                        , False
                        )

                    _ ->
                        ( { model | cursor = point }, Cmd.none, False )

        LeftClick point ->
            if List.isEmpty model.layers then
                ( model, Cmd.none, False )

            else
                Tool.update ToolMsg (Tool.LeftClick point) model

        ZoomDelta delta shiftPressed ctrlPressed ->
            if shiftPressed then
                Tool.update ToolMsg (Tool.ShiftScroll delta) model

            else if ctrlPressed then
                Tool.update ToolMsg (Tool.CtrlScroll delta) model

            else
                ( { model | transform = zoomTransform model.transform delta }, Cmd.none, False )

        SetTransform transform ->
            ( { model | transform = transform }, Cmd.none, False )

        CycleLayers ->
            ( { model | layers = cycle model.layers }, Cmd.none, False )

        AddLayer layerId ->
            ( { model | layers = newLayer layerId :: model.layers }, Cmd.none, True )

        Focus ->
            ( { model | focused = True }, Cmd.none, False )

        Unfocus ->
            ( { model | focused = False }, Cmd.none, False )

        VisualElementMsg (Visual.Click element) ->
            case element of
                Visual.Circle conductor point radius ->
                    ( model, Cmd.none, True )

                -- TODO
                Visual.ConstructionCircle point radius _ ->
                    ( model, Cmd.none, True )

                Visual.Square conductor point width _ ->
                    ( model, Cmd.none, True )

                Visual.SquareOutline conductor point width _ ->
                    ( model, Cmd.none, True )

                Visual.ConstructionSquare _ _ _ ->
                    ( model, Cmd.none, False )

                Visual.Line conductor p1 p2 _ ->
                    ( model, Cmd.none, True )

                Visual.DashedLine conductor p1 p2 _ ->
                    ( model, Cmd.none, True )

                Visual.ConstructionLine _ _ _ ->
                    ( model, Cmd.none, False )

                Visual.ConstructionSegment constructionPoints ->
                    ( model, Cmd.none, False )

                Visual.ConstructionCrosshair constructionPoint ->
                    ( model, Cmd.none, False )

        ToolMsg toolMsg ->
            Tool.update ToolMsg toolMsg model

        FormMsg formMsg ->
            Form.update FormMsg formMsg model



-- VIEWS
