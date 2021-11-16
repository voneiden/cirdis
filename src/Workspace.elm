module Workspace exposing (..)

import Common exposing (Dragging, Point, Radius, ShiftPressed, Thickness, Width, cycle, distanceToPoint, fromPoint, unique)
import Html exposing (Attribute)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE



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
    }


defaultModel : Model
defaultModel =
    { layers = []
    , cursor = Point 0 0
    , focused = False
    , transform = { x = 0, y = 0, z = 1 }
    , canvas = { width = 0, height = 0 }
    , radius = 10
    , thickness = 5
    , tool = SelectTool Nothing
    , conductors = []
    , nextNetId = 1
    , snapDistance = 10
    , autoNetColor = ""
    , highlightNets = []
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


activeLayerSurfaceConductors : Model -> List SurfaceConductor
activeLayerSurfaceConductors model =
    Maybe.withDefault [] (Maybe.map (\l -> l.conductors) (List.head model.layers))


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


type alias Canvas =
    { width : Float
    , height : Float
    }


type Net
    = NoNet Int
    | AutoNet Int
    | CustomNet String String


netColor : List Net -> Net -> String
netColor highlightedNets net =
    if List.member net highlightedNets then
        case net of
            NoNet _ ->
                "cyan"

            AutoNet _ ->
                "cyan"

            CustomNet _ c ->
                "cyan"
        -- todo?

    else
        case net of
            NoNet _ ->
                "red"

            AutoNet _ ->
                "grey"

            CustomNet _ c ->
                c


netsConductors : Model -> List Net -> List Conductor
netsConductors model nets =
    List.filter (\c -> List.member (conductorNet c) nets) (allConductors model)


type Conductor
    = Surface SurfaceConductor
    | Through ThroughConductor


allConductors : Model -> List Conductor
allConductors model =
    List.map Through model.conductors ++ List.concatMap (\layer -> List.map Surface layer.conductors) model.layers


conductorNet : Conductor -> Net
conductorNet conductor =
    case conductor of
        Through tht ->
            throughConductorNet tht

        Surface smt ->
            surfaceConductorNet smt


{-| Find out the shortest distance from a given point to a conductor
-}
distanceToConductor : Point -> Conductor -> Maybe ( Float, Point, Conductor )
distanceToConductor point conductor =
    conductorPoints conductor
        |> List.map (\conductorPoint -> ( distanceToPoint point conductorPoint, conductorPoint, conductor ))
        |> List.sortBy (\( distance, _, _ ) -> distance)
        |> List.head


conductorPoints : Conductor -> List Point
conductorPoints conductor =
    case conductor of
        Through tht ->
            throughConductorPoints tht

        Surface smt ->
            surfaceConductorPoints smt


type ThroughConductor
    = ThroughPad Point Radius Net


throughConductorNet : ThroughConductor -> Net
throughConductorNet tht =
    case tht of
        ThroughPad _ _ net ->
            net


throughConductorPoints : ThroughConductor -> List Point
throughConductorPoints tht =
    case tht of
        ThroughPad point _ _ ->
            [ point ]


type SurfaceConductor
    = Trace (List TracePoint) Net
    | SurfacePad Point Width Net
    | Zone (List Point) Net


surfaceConductorNet : SurfaceConductor -> Net
surfaceConductorNet smt =
    case smt of
        Trace _ net ->
            net

        SurfacePad _ _ net ->
            net

        Zone _ net ->
            net


surfaceConductorPoints : SurfaceConductor -> List Point
surfaceConductorPoints smt =
    case smt of
        Trace tracePoints _ ->
            List.map .point tracePoints

        SurfacePad point _ _ ->
            [ point ]

        Zone points _ ->
            points


type alias TracePoint =
    { point : Point
    , thickness : Float
    }


pointToTracePoint : Point -> Float -> TracePoint
pointToTracePoint point thickness =
    { point = point, thickness = thickness }


type ConstructionPoint a
    = FreePoint Point a
    | SnapPoint Point Conductor a


constructionPointPoint : ConstructionPoint a -> Point
constructionPointPoint sp =
    case sp of
        FreePoint p _ ->
            p

        SnapPoint p _ _ ->
            p


constructionPointConductor : ConstructionPoint a -> Maybe Conductor
constructionPointConductor sp =
    case sp of
        FreePoint p _ ->
            Nothing

        SnapPoint _ c _ ->
            Just c


constructionPointA : ConstructionPoint a -> a
constructionPointA cp =
    case cp of
        FreePoint _ a ->
            a

        SnapPoint _ _ a ->
            a


constructionPointToMaybeConductor : ConstructionPoint a -> Maybe Conductor
constructionPointToMaybeConductor cp =
    case cp of
        FreePoint _ _ ->
            Nothing

        SnapPoint _ conductor _ ->
            Just conductor


constructionPointsToConductors : List (ConstructionPoint a) -> List Conductor
constructionPointsToConductors cps =
    List.filterMap constructionPointToMaybeConductor cps


constructionPointToTracePoint : ConstructionPoint Thickness -> TracePoint
constructionPointToTracePoint cp =
    { point = constructionPointPoint cp, thickness = constructionPointA cp }


constructionPointsToTracePoints : List (ConstructionPoint Thickness) -> List TracePoint
constructionPointsToTracePoints cps =
    List.map constructionPointToTracePoint cps


constructionPointsToTrace : List (ConstructionPoint Thickness) -> Net -> SurfaceConductor
constructionPointsToTrace points net =
    -- todo net
    Trace (constructionPointsToTracePoints points) net


type Tool
    = SelectTool (Maybe Conductor)
    | CreateTraceTool (List (ConstructionPoint Thickness))
    | CreateSurfacePadTool
    | CreateThroughPadTool
    | CreateZoneTool


type MergeNet
    = MergeOk Net (List Conductor)
    | MergeConflict (List Net) (List Conductor)
    | MergeNoNet (List Conductor)


isCustomNet : Net -> Bool
isCustomNet net =
    case net of
        CustomNet _ _ ->
            True

        _ ->
            False


isAutoNet : Net -> Bool
isAutoNet net =
    case net of
        AutoNet _ ->
            True

        _ ->
            False


isNoNet : Net -> Bool
isNoNet net =
    case net of
        NoNet _ ->
            True

        _ ->
            False


mergeNets : Model -> List Conductor -> MergeNet
mergeNets model snappedConductors =
    let
        nets =
            List.map conductorNet snappedConductors
                |> unique

        conductors =
            netsConductors model nets

        ( customNets, autoNets ) =
            List.partition isCustomNet (List.filter (not << isNoNet) nets)
    in
    case ( customNets, autoNets ) of
        ( [ customNet ], _ ) ->
            MergeOk customNet conductors

        ( [], autoNet :: _ ) ->
            MergeOk autoNet conductors

        ( _ :: _, _ ) ->
            MergeConflict customNets conductors

        ( [], [] ) ->
            MergeNoNet conductors



-- UPDATE


type Msg
    = SetCursor Point ( Float, Float ) Dragging
    | LeftClick Point
    | ZoomDelta Float ShiftPressed
    | SetTool Tool
    | ResetTool
    | SetTransform Transform
    | AddLayer Int
    | CycleLayers
    | Focus
    | Unfocus
    | ClickConductor ConductorSelection


type ConductorSelection
    = ConductorSelection Conductor
    | TraceSegmentSelection SurfaceConductor Point Point


update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update msg model =
    case msg of
        SetCursor point ( dx, dy ) dragging ->
            if dragging then
                ( { model | cursor = point, transform = translateTransform model.transform dx dy }, Cmd.none, False )

            else
                case model.tool of
                    CreateTraceTool cps ->
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
            let
                snapPoint =
                    snapTo model.snapDistance point model.conductors (activeLayerSurfaceConductors model)
            in
            if List.isEmpty model.layers then
                ( model, Cmd.none, False )

            else
                case model.tool of
                    CreateTraceTool points ->
                        case snapPoint model.thickness of
                            SnapPoint p c t ->
                                let
                                    newPoints =
                                        points ++ [ SnapPoint p c t ]
                                in
                                if List.isEmpty points then
                                    ( { model | tool = CreateTraceTool newPoints }
                                        |> createTraceToHighlightNets newPoints
                                    , Cmd.none
                                    , True
                                    )

                                else
                                    let
                                        mergedNet =
                                            mergeNets model (constructionPointsToConductors newPoints)
                                    in
                                    case mergedNet of
                                        MergeOk net conductors ->
                                            ( List.foldl (updateConductorNet net) model conductors
                                                |> addSurfaceConductor (constructionPointsToTrace newPoints net)
                                                |> resetTool
                                                |> createTraceToHighlightNets newPoints
                                            , Cmd.none
                                            , True
                                            )

                                        MergeConflict nets conductors ->
                                            -- TODO show somekind of conflict resolution thing
                                            ( resetTool model, Cmd.none, True )

                                        MergeNoNet conductors ->
                                            let
                                                net =
                                                    AutoNet model.nextNetId
                                            in
                                            ( List.foldl (updateConductorNet net) model conductors
                                                |> addSurfaceConductor (constructionPointsToTrace newPoints net)
                                                |> resetTool
                                                |> incrementNextNetId
                                                |> createTraceToHighlightNets newPoints
                                            , Cmd.none
                                            , True
                                            )

                            FreePoint p t ->
                                let
                                    t2 =
                                        points ++ [ FreePoint p t ]
                                in
                                if List.isEmpty points then
                                    ( model, Cmd.none, False )

                                else
                                    ( { model | tool = CreateTraceTool t2 }, Cmd.none, False )

                    CreateThroughPadTool ->
                        ( addThroughConductor (ThroughPad point model.radius) model, Cmd.none, True )

                    CreateSurfacePadTool ->
                        ( addSurfaceConductorNoNet (SurfacePad point (model.radius * 2)) model, Cmd.none, True )

                    _ ->
                        ( model, Cmd.none, False )

        ZoomDelta delta shiftPressed ->
            if shiftPressed then
                case model.tool of
                    CreateThroughPadTool ->
                        ( updateRadius delta model, Cmd.none, False )

                    CreateSurfacePadTool ->
                        ( updateRadius delta model, Cmd.none, False )

                    _ ->
                        ( model, Cmd.none, False )

            else
                ( { model | transform = zoomTransform model.transform delta }, Cmd.none, False )

        SetTool tool ->
            case ( tool, model.tool ) of
                ( CreateTraceTool [], CreateTraceTool trace ) ->
                    ( { model
                        | tool =
                            CreateTraceTool
                                (trace
                                    |> List.reverse
                                    |> List.drop 1
                                    |> List.reverse
                                )
                      }
                    , Cmd.none
                    , False
                    )

                _ ->
                    ( { model | tool = tool }, Cmd.none, False )

        ResetTool ->
            ( resetTool model, Cmd.none, False )

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

        ClickConductor conductorSelection ->
            case conductorSelection of
                ConductorSelection conductor ->
                    ( { model | highlightNets = [ conductorNet conductor ] }, Cmd.none, False )

                TraceSegmentSelection surfaceConductor p1 p2 ->
                    ( { model | highlightNets = [ surfaceConductorNet surfaceConductor ] }, Cmd.none, False )


createTraceToHighlightNets : List (ConstructionPoint Thickness) -> Model -> Model
createTraceToHighlightNets points model =
    { model | highlightNets = List.map conductorNet (constructionPointsToConductors points) }


updateRadius : Float -> Model -> Model
updateRadius delta model =
    let
        multiplier =
            1.25

        newZ =
            if delta < 0 then
                model.radius * multiplier

            else
                model.radius / multiplier
    in
    if newZ > 0.9 && newZ < 1.1 then
        { model | radius = 1 }

    else
        { model | radius = newZ }


resetTool : Model -> Model
resetTool model =
    let
        tool =
            case model.tool of
                SelectTool _ ->
                    SelectTool Nothing

                CreateTraceTool _ ->
                    CreateTraceTool []

                CreateSurfacePadTool ->
                    CreateSurfacePadTool

                CreateThroughPadTool ->
                    CreateThroughPadTool

                CreateZoneTool ->
                    CreateZoneTool
    in
    { model | tool = tool }


incrementNextNetId : Model -> Model
incrementNextNetId model =
    { model | nextNetId = model.nextNetId + 1 }


addThroughConductor : (Net -> ThroughConductor) -> Model -> Model
addThroughConductor toConductor model =
    case toConductor (NoNet model.nextNetId) of
        ThroughPad point radius net ->
            { model | nextNetId = model.nextNetId + 1, conductors = ThroughPad point radius net :: model.conductors }


addSurfaceConductorNoNet : (Net -> SurfaceConductor) -> Model -> Model
addSurfaceConductorNoNet toSurfaceConductor model =
    case model.layers of
        layer :: others ->
            let
                updatedLayer =
                    { layer | conductors = toSurfaceConductor (NoNet model.nextNetId) :: layer.conductors }
            in
            { model | layers = updatedLayer :: others, nextNetId = model.nextNetId + 1 }

        _ ->
            model


addSurfaceConductor : SurfaceConductor -> Model -> Model
addSurfaceConductor surfaceConductor model =
    case model.layers of
        layer :: others ->
            let
                updatedLayer =
                    { layer | conductors = surfaceConductor :: layer.conductors }
            in
            { model | layers = updatedLayer :: others }

        _ ->
            model


updateConductorNet : Net -> Conductor -> Model -> Model
updateConductorNet net conductor model =
    case conductor of
        Surface surfaceConductor ->
            updateSurfaceConductorNet net surfaceConductor model

        Through throughConductor ->
            let
                updatedThroughConductor =
                    case throughConductor of
                        ThroughPad point radius _ ->
                            ThroughPad point radius net
            in
            { model
                | conductors =
                    List.map
                        (\c ->
                            if c == throughConductor then
                                updatedThroughConductor

                            else
                                c
                        )
                        model.conductors
            }


updateSurfaceConductorNet : Net -> SurfaceConductor -> Model -> Model
updateSurfaceConductorNet net surfaceConductor model =
    case model.layers of
        layer :: others ->
            let
                updatedSurfaceConductor =
                    case surfaceConductor of
                        Trace tracePoints _ ->
                            Trace tracePoints net

                        SurfacePad point width _ ->
                            SurfacePad point width net

                        Zone points _ ->
                            Zone points net

                updatedConductors =
                    List.map
                        (\c ->
                            if c == surfaceConductor then
                                updatedSurfaceConductor

                            else
                                c
                        )
                        layer.conductors

                updatedLayer =
                    { layer | conductors = updatedConductors }
            in
            { model | layers = updatedLayer :: others }

        _ ->
            model



-- VIEWS


{-| Display anything and everything related to the active tool
-}
viewTool : Model -> Svg Msg
viewTool model =
    case model.tool of
        CreateTraceTool cps ->
            viewTraceWithConstruction model (constructionPointsToTracePoints cps)

        CreateThroughPadTool ->
            viewThroughConductor model.highlightNets <| ThroughPad model.cursor model.radius (NoNet model.nextNetId)

        CreateSurfacePadTool ->
            viewSurfaceConductor model.highlightNets False <| SurfacePad model.cursor (model.radius * 2) (NoNet model.nextNetId)

        _ ->
            Svg.text ""


toolToString : Tool -> String
toolToString tool =
    case tool of
        SelectTool _ ->
            "select"

        CreateTraceTool _ ->
            "trace"

        CreateSurfacePadTool ->
            "surface"

        CreateThroughPadTool ->
            "through"

        CreateZoneTool ->
            "zone"


viewThroughConductor : List Net -> ThroughConductor -> Svg Msg
viewThroughConductor highlightNets throughConductor =
    case throughConductor of
        ThroughPad point radius net ->
            Svg.circle
                [ SvgA.cx <| String.fromFloat point.x
                , SvgA.cy <| String.fromFloat point.y
                , SvgA.r <| String.fromFloat radius
                , SvgA.fill (netColor highlightNets net)
                , SvgE.onClick (ClickConductor (ConductorSelection (Through throughConductor)))
                , SvgA.class "clickable"
                ]
                []


viewSurfaceConductors : List Net -> List Layer -> List (Svg Msg)
viewSurfaceConductors highlightNets layers =
    case layers of
        layer :: hiddenLayers ->
            let
                highlightedHiddenSurfaceConductors =
                    List.concatMap .conductors hiddenLayers
                        |> List.filter (\c -> List.member (surfaceConductorNet c) highlightNets)
            in
            List.map (viewSurfaceConductor highlightNets False) layer.conductors
                ++ List.map (viewSurfaceConductor highlightNets True) highlightedHiddenSurfaceConductors

        [] ->
            []


viewSurfaceConductor : List Net -> Bool -> SurfaceConductor -> Svg Msg
viewSurfaceConductor highlightNets hidden surfaceConductor =
    case surfaceConductor of
        Trace tracePoints net ->
            let
                attrs =
                    if hidden then
                        [ SvgA.strokeDasharray "5,15" ]

                    else
                        []
            in
            viewTrace (\p1 p2 -> ClickConductor (TraceSegmentSelection surfaceConductor p1 p2)) (netColor highlightNets net) attrs tracePoints

        SurfacePad point width net ->
            let
                half =
                    width / 2

                attrs =
                    if hidden then
                        [ SvgA.stroke <| netColor highlightNets net
                        , SvgA.strokeWidth "2px"
                        , SvgA.fill "none"
                        ]

                    else
                        [ SvgA.fill <| netColor highlightNets net ]
            in
            Svg.rect
                ([ SvgA.x <| String.fromFloat (point.x - half)
                 , SvgA.y <| String.fromFloat (point.y - half)
                 , SvgA.width <| String.fromFloat width
                 , SvgA.height <| String.fromFloat width
                 ]
                    ++ attrs
                )
                []

        Zone points net ->
            Svg.text "ZONE"


viewTrace : (Point -> Point -> Msg) -> String -> List (Attribute Msg) -> List TracePoint -> Svg Msg
viewTrace toClickMsg color attrs points =
    Svg.g [] (viewTraceSegmented (Just toClickMsg) color attrs points)


viewTraceWithConstruction : Model -> List TracePoint -> Svg Msg
viewTraceWithConstruction model points =
    let
        constructionTrace =
            if model.focused then
                viewConstructionTrace model points

            else
                []
    in
    Svg.g [] <|
        viewTraceSegmented Nothing (netColor [] (NoNet 0)) [] points
            ++ constructionTrace


viewCrosshair : ConstructionPoint a -> Svg Msg
viewCrosshair point =
    let
        length =
            35

        ( center, color ) =
            case point of
                SnapPoint p _ _ ->
                    ( p, "lime" )

                FreePoint p _ ->
                    ( p, "red" )
    in
    Svg.g []
        [ Svg.line
            [ SvgA.x1 <| String.fromFloat <| center.x - length
            , SvgA.x2 <| String.fromFloat <| center.x + length
            , SvgA.y1 <| String.fromFloat <| center.y
            , SvgA.y2 <| String.fromFloat <| center.y
            , SvgA.stroke color
            ]
            []
        , Svg.line
            [ SvgA.x1 <| String.fromFloat <| center.x
            , SvgA.x2 <| String.fromFloat <| center.x
            , SvgA.y1 <| String.fromFloat <| center.y - length
            , SvgA.y2 <| String.fromFloat <| center.y + length
            , SvgA.stroke color
            ]
            []
        ]


viewTraceSegment : Maybe (Point -> Point -> Msg) -> String -> List (Attribute Msg) -> TracePoint -> TracePoint -> Svg Msg
viewTraceSegment maybeToClickMsg color attrs start end =
    let
        d =
            SvgA.d <|
                String.join " " <|
                    [ fromPoint "M" start.point
                    , fromPoint "L" end.point
                    ]
    in
    Svg.path
        ([ d
         , SvgA.fill "none"
         , SvgA.stroke color
         , SvgA.strokeWidth (String.fromFloat start.thickness)
         , SvgA.strokeLinecap "round"
         ]
            ++ Maybe.withDefault [] (Maybe.map (\f -> [ SvgE.onClick (f start.point end.point), SvgA.class "clickable" ]) maybeToClickMsg)
            ++ attrs
        )
        []


viewTraceSegmented : Maybe (Point -> Point -> Msg) -> String -> List (Attribute Msg) -> List TracePoint -> List (Svg Msg)
viewTraceSegmented maybeToClickMsg color attrs tracePoints =
    case tracePoints of
        start :: end :: rest ->
            viewTraceSegment maybeToClickMsg color attrs start end :: viewTraceSegmented maybeToClickMsg color attrs (end :: rest)

        _ ->
            [ Svg.text "" ]



-- todo this one has confusing name with the other similar function, fix


viewConstructionTrace : Model -> List TracePoint -> List (Svg Msg)
viewConstructionTrace model tracePoints =
    let
        cp =
            snapTo model.snapDistance model.cursor model.conductors (activeLayerSurfaceConductors model) model.thickness

        point =
            case cp of
                SnapPoint p _ _ ->
                    p

                FreePoint p _ ->
                    p
    in
    case List.reverse tracePoints of
        last :: _ ->
            [ viewTraceSegment Nothing (netColor [] (NoNet 0)) [] last (pointToTracePoint point model.thickness)
            , viewCrosshair cp
            ]

        _ ->
            [ viewCrosshair cp ]



-- UTILITY


snapTo : Float -> Point -> List ThroughConductor -> List SurfaceConductor -> (a -> ConstructionPoint a)
snapTo maxSnapDistance point thts smts =
    let
        closest =
            List.map Through thts
                ++ List.map Surface smts
                |> List.filterMap (distanceToConductor point)
                |> List.sortBy (\( d, _, _ ) -> d)
                |> List.head
    in
    case closest of
        Just ( conductorDistance, conductorPoint, conductor ) ->
            if snaps conductor conductorDistance maxSnapDistance then
                SnapPoint conductorPoint conductor

            else
                FreePoint point

        Nothing ->
            FreePoint point


snaps : Conductor -> Float -> Float -> Bool
snaps conductor conductorDistance defaultSnapDistance =
    case conductor of
        Surface _ ->
            conductorDistance < defaultSnapDistance

        Through tc ->
            case tc of
                ThroughPad _ radius _ ->
                    conductorDistance <= radius
