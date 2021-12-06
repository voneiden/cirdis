module Workspace exposing (..)

import Common exposing (Color, CtrlPressed, Dimension, Dragging, Point, Radius, ReferenceFrame, ShiftPressed, Thickness, Width, chainUpdate3, cycle)
import Conductor exposing (..)
import Form
import Tool exposing (Tool)
import Visual



-- MODEL


type alias Model =
    { layers : List Layer
    , cursor : Point
    , constructionCursor : ConstructionPoint ()
    , focused : Bool
    , transform : Transform
    , canvas : Canvas
    , radius : Float -- fixme this is tool related
    , thickness : Float -- fixme this is tool related
    , tool : Tool
    , conductors : List ThroughConductor
    , nextNetId : Int -- Running id for nets
    , snapDistance : Float
    , autoNetColor : String
    , ref : Maybe ReferenceFrame
    , form : Form.Form
    , dimensions : List Dimension
    , lastVisualMouseOver : Visual.VisualElement
    }


defaultModel : Model
defaultModel =
    { layers = []
    , cursor = Point 0 0
    , constructionCursor = FreePoint (Point 0 0) ()
    , focused = False
    , transform = defaultTransform
    , canvas = { width = 0, height = 0 }
    , radius = 10
    , thickness = 5
    , tool = Tool.SelectTool Conductor.NoInteraction Conductor.NoInteraction
    , conductors = []
    , nextNetId = 1
    , snapDistance = 10
    , autoNetColor = ""
    , ref = Nothing
    , form = Form.WelcomeForm
    , dimensions = []
    , lastVisualMouseOver = Visual.Background
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
            let
                cp =
                    snapTo model.snapDistance point model.conductors (activeLayerSurfaceConductors model) ()
            in
            if dragging then
                ( { model | cursor = point, constructionCursor = cp, transform = translateTransform model.transform dx dy }, Cmd.none, False )

            else
                ( { model | cursor = point, constructionCursor = cp }, Cmd.none, False )

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

        VisualElementMsg visualElementMsg ->
            case visualElementMsg of
                Visual.Click element ->
                    case element of
                        Visual.Line conductor p1 p2 _ ->
                            ( { model | tool = Tool.setToolSelection model.tool (Conductor.SegmentInteraction conductor p1 p2) }, Cmd.none, True )

                        Visual.Background ->
                            ( { model | tool = Tool.setToolSelection model.tool Conductor.NoInteraction }, Cmd.none, False )

                        _ ->
                            ( model, Cmd.none, False )

                Visual.MouseOver visualElement ->
                    let
                        newModel =
                            { model | lastVisualMouseOver = visualElement }
                    in
                    case visualElement of
                        Visual.Line conductor p1 p2 _ ->
                            ( { newModel | tool = Tool.setToolHighlight model.tool (Conductor.SegmentInteraction conductor p1 p2) }, Cmd.none, True )

                        Visual.Circle conductor point _ _ ->
                            ( { newModel | tool = Tool.setToolHighlight model.tool (Conductor.PointInteraction [ conductor ] point) }, Cmd.none, True )

                        Visual.Background ->
                            ( { newModel | tool = Tool.setToolHighlight model.tool Conductor.NoInteraction }, Cmd.none, False )

                        _ ->
                            ( newModel, Cmd.none, False )

                Visual.MouseOut visualElement ->
                    Debug.todo "implement mouse out"

        ToolMsg toolMsg ->
            Tool.update ToolMsg toolMsg model
                |> chainUpdate3 (\m -> update (VisualElementMsg <| Visual.MouseOver <| m.lastVisualMouseOver) m)

        FormMsg formMsg ->
            Form.update FormMsg formMsg model



-- VIEWS
