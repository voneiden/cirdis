module Workspace exposing (..)

import Common exposing (Dragging, Point, Radius, ShiftPressed, Thickness, Width, cycle, distanceToPoint, fromPoint)
import Svg exposing (Svg)
import Svg.Attributes as SvgA



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
    , tool = SelectConductor Nothing
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
    = NoNet
    | AutoNet Int
    | CustomNet String String


netColor : Net -> String
netColor net =
    case net of
        NoNet ->
            "red"

        AutoNet _ ->
            "grey"

        CustomNet _ c ->
            c


type Conductor
    = Surface SurfaceConductor
    | Through ThroughConductor


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
    = SelectConductor (Maybe Conductor)
    | SelectNet (Maybe Net)
    | CreateTrace (List (ConstructionPoint Thickness))
    | CreateSurfacePad
    | CreateThroughPad
    | CreateZone


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
        NoNet ->
            True

        _ ->
            False


mergeNets : List Conductor -> MergeNet
mergeNets conductors =
    let
        nets =
            List.map conductorNet conductors

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCursor point ( dx, dy ) dragging ->
            if dragging then
                ( { model | cursor = point, transform = translateTransform model.transform dx dy }, Cmd.none )

            else
                ( { model | cursor = point }, Cmd.none )

        LeftClick point ->
            let
                snapPoint =
                    snapTo model.snapDistance point model.conductors (activeLayerSurfaceConductors model)
            in
            case model.tool of
                CreateTrace points ->
                    case snapPoint model.thickness of
                        SnapPoint p c t ->
                            let
                                newPoints =
                                    points ++ [ SnapPoint p c t ]
                            in
                            if List.isEmpty points then
                                ( { model | tool = CreateTrace newPoints }, Cmd.none )

                            else
                                let
                                    mergedNet =
                                        mergeNets (constructionPointsToConductors newPoints)
                                in
                                case mergedNet of
                                    MergeOk net conductors ->
                                        ( List.foldl (updateConductorNet net) model conductors
                                            |> addSurfaceConductor (constructionPointsToTrace newPoints net)
                                            |> resetTool
                                        , Cmd.none
                                        )

                                    MergeConflict nets conductors ->
                                        -- TODO show somekind of conflict resolution thing
                                        ( resetTool model, Cmd.none )

                                    MergeNoNet conductors ->
                                        let
                                            net =
                                                AutoNet model.nextNetId
                                        in
                                        ( List.foldl (updateConductorNet net) model conductors
                                            |> addSurfaceConductor (constructionPointsToTrace newPoints net)
                                            |> resetTool
                                            |> incrementNextNetId
                                        , Cmd.none
                                        )

                        FreePoint p t ->
                            let
                                t2 =
                                    points ++ [ FreePoint p t ]
                            in
                            ( { model | tool = CreateTrace t2 }, Cmd.none )

                CreateThroughPad ->
                    ( addThroughConductor (ThroughPad point model.radius) model, Cmd.none )

                CreateSurfacePad ->
                    ( addSurfaceConductorAutoNet (SurfacePad point (model.radius * 2)) model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ZoomDelta delta shiftPressed ->
            if shiftPressed then
                case model.tool of
                    CreateThroughPad ->
                        ( updateRadius delta model, Cmd.none )

                    CreateSurfacePad ->
                        ( updateRadius delta model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else
                ( { model | transform = zoomTransform model.transform delta }, Cmd.none )

        SetTool tool ->
            case ( tool, model.tool ) of
                ( CreateTrace [], CreateTrace trace ) ->
                    ( { model
                        | tool =
                            CreateTrace
                                (trace
                                    |> List.reverse
                                    |> List.drop 1
                                    |> List.reverse
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | tool = tool }, Cmd.none )

        ResetTool ->
            ( resetTool model, Cmd.none )

        SetTransform transform ->
            ( { model | transform = transform }, Cmd.none )

        CycleLayers ->
            ( { model | layers = cycle model.layers }, Cmd.none )

        AddLayer layerId ->
            ( { model | layers = newLayer layerId :: model.layers }, Cmd.none )

        Focus ->
            ( { model | focused = True }, Cmd.none )

        Unfocus ->
            ( { model | focused = False }, Cmd.none )


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
                SelectConductor _ ->
                    SelectConductor Nothing

                SelectNet _ ->
                    SelectNet Nothing

                CreateTrace _ ->
                    CreateTrace []

                CreateSurfacePad ->
                    CreateSurfacePad

                CreateThroughPad ->
                    CreateThroughPad

                CreateZone ->
                    CreateZone
    in
    { model | tool = tool }


incrementNextNetId : Model -> Model
incrementNextNetId model =
    { model | nextNetId = model.nextNetId + 1 }


addThroughConductor : (Net -> ThroughConductor) -> Model -> Model
addThroughConductor toConductor model =
    case toConductor NoNet of
        ThroughPad point radius _ ->
            { model | nextNetId = model.nextNetId + 1, conductors = toConductor NoNet :: model.conductors }


addSurfaceConductorAutoNet : (Net -> SurfaceConductor) -> Model -> Model
addSurfaceConductorAutoNet toSurfaceConductor model =
    case model.layers of
        layer :: others ->
            let
                updatedLayer =
                    { layer | conductors = toSurfaceConductor NoNet :: layer.conductors }
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
        CreateTrace cps ->
            viewTraceWithConstruction model (constructionPointsToTracePoints cps)

        CreateThroughPad ->
            viewThroughConductor <| ThroughPad model.cursor model.radius NoNet

        CreateSurfacePad ->
            viewSurfaceConductor <| SurfacePad model.cursor (model.radius * 2) NoNet

        _ ->
            Svg.text ""


viewThroughConductor : ThroughConductor -> Svg Msg
viewThroughConductor throughConductor =
    case throughConductor of
        ThroughPad point radius net ->
            Svg.circle
                [ SvgA.cx <| String.fromFloat point.x
                , SvgA.cy <| String.fromFloat point.y
                , SvgA.r <| String.fromFloat radius
                , SvgA.fill (netColor net)
                ]
                []


viewSurfaceConductor : SurfaceConductor -> Svg Msg
viewSurfaceConductor surfaceConductor =
    case surfaceConductor of
        Trace tracePoints net ->
            viewTrace (netColor net) tracePoints

        SurfacePad point width net ->
            let
                half =
                    width / 2
            in
            Svg.rect
                [ SvgA.x <| String.fromFloat (point.x - half)
                , SvgA.y <| String.fromFloat (point.y - half)
                , SvgA.width <| String.fromFloat width
                , SvgA.height <| String.fromFloat width
                , SvgA.fill <| netColor net
                ]
                []

        Zone points net ->
            Svg.text "ZONE"


viewTrace : String -> List TracePoint -> Svg Msg
viewTrace color points =
    Svg.g [] (viewTraceSegmented color points)


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
        viewTraceSegmented (netColor NoNet) points
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


viewTracePoints : String -> List TracePoint -> Svg Msg
viewTracePoints color tracePoints =
    case tracePoints of
        start :: rest ->
            let
                d =
                    SvgA.d <|
                        String.join " " <|
                            [ fromPoint "M" start.point ]
                                ++ List.map (\tp -> fromPoint "L" tp.point) rest
            in
            Svg.path
                [ d
                , SvgA.fill "none"
                , SvgA.stroke color
                , SvgA.strokeWidth (String.fromFloat start.thickness)
                , SvgA.strokeLinecap "round"
                ]
                []

        _ ->
            Svg.text ""


viewTraceSegmented : String -> List TracePoint -> List (Svg Msg)
viewTraceSegmented color tracePoints =
    -- TODO add some intelligence here and merge lines that have the same thickness
    -- use takeWhile here
    case tracePoints of
        start :: end :: rest ->
            viewTracePoints color [ start, end ] :: viewTraceSegmented color (end :: rest)

        _ ->
            [ Svg.text "" ]


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
            [ viewTracePoints (netColor NoNet) [ last, pointToTracePoint point model.thickness ]
            , viewCrosshair cp
            ]

        _ ->
            [ viewCrosshair cp ]


viewMaybeLayerSurfaceConductors : Maybe Layer -> List (Svg Msg)
viewMaybeLayerSurfaceConductors maybeLayer =
    case maybeLayer of
        Just layer ->
            List.map viewSurfaceConductor layer.conductors

        Nothing ->
            []



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
            if conductorDistance < maxSnapDistance then
                SnapPoint conductorPoint conductor

            else
                FreePoint point

        Nothing ->
            FreePoint point
