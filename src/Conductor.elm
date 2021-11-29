module Conductor exposing (..)

import Common exposing (Pad, Point, Radius, Thickness, Width, distanceToPoint, unique)


type Net
    = NoNet Int
    | AutoNet Int
    | CustomNet String String


netsConductors : List Conductor -> List Net -> List Conductor
netsConductors conductors nets =
    List.filter (\c -> List.member (conductorNet c) nets) conductors


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


mergeNets : ModelConductors a b -> List Conductor -> MergeNet
mergeNets model snappedConductors =
    let
        nets =
            List.map conductorNet snappedConductors
                |> unique

        conductors =
            netsConductors (allConductors model) nets

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
    = ThroughPad Pad Point Radius Net


throughConductorNet : ThroughConductor -> Net
throughConductorNet tht =
    case tht of
        ThroughPad _ _ _ net ->
            net


throughConductorPoints : ThroughConductor -> List Point
throughConductorPoints tht =
    case tht of
        ThroughPad _ point _ _ ->
            [ point ]


type SurfaceConductor
    = Trace (List TracePoint) Net
    | SurfacePad Pad Point Width Net
    | Zone (List Point) Net


surfaceConductorNet : SurfaceConductor -> Net
surfaceConductorNet smt =
    case smt of
        Trace _ net ->
            net

        SurfacePad _ _ _ net ->
            net

        Zone _ net ->
            net


surfaceConductorPoints : SurfaceConductor -> List Point
surfaceConductorPoints smt =
    case smt of
        Trace tracePoints _ ->
            List.map .point tracePoints

        SurfacePad _ point _ _ ->
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



-- UPDATE


addSurfaceConductor : SurfaceConductor -> ModelConductors a b -> ModelConductors a b
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


addThroughConductor : (Net -> ThroughConductor) -> ModelConductors a b -> ModelConductors a b
addThroughConductor toConductor model =
    case toConductor (NoNet model.nextNetId) of
        ThroughPad pad point radius net ->
            { model | nextNetId = model.nextNetId + 1, conductors = ThroughPad pad point radius net :: model.conductors }


addSurfaceConductorNoNet : (Net -> SurfaceConductor) -> ModelConductors a b -> ModelConductors a b
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


updateConductorNet : Net -> Conductor -> ModelConductors a b -> ModelConductors a b
updateConductorNet net conductor model =
    case conductor of
        Surface surfaceConductor ->
            updateSurfaceConductorNet net surfaceConductor model

        Through throughConductor ->
            let
                updatedThroughConductor =
                    case throughConductor of
                        ThroughPad pad point radius _ ->
                            ThroughPad pad point radius net
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


updateSurfaceConductorNet : Net -> SurfaceConductor -> ModelConductors a b -> ModelConductors a b
updateSurfaceConductorNet net surfaceConductor model =
    let
        hasConductor layer =
            List.member surfaceConductor layer.conductors

        updateLayer layer =
            if hasConductor layer then
                let
                    updatedSurfaceConductor =
                        case surfaceConductor of
                            Trace tracePoints _ ->
                                Trace tracePoints net

                            SurfacePad pad point width _ ->
                                SurfacePad pad point width net

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
                in
                { layer | conductors = updatedConductors }

            else
                layer
    in
    { model
        | layers =
            List.map updateLayer model.layers
    }



-- Model util


type alias ModelConductors a b =
    { a | nextNetId : Int, conductors : List ThroughConductor, layers : List { b | conductors : List SurfaceConductor } }


allConductors : ModelConductors a b -> List Conductor
allConductors model =
    List.map Through model.conductors ++ List.concatMap (\layer -> List.map Surface layer.conductors) model.layers



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
                ThroughPad _ _ radius _ ->
                    conductorDistance <= radius


activeLayerSurfaceConductors : ModelConductors a b -> List SurfaceConductor
activeLayerSurfaceConductors model =
    Maybe.withDefault [] (Maybe.map (\l -> l.conductors) (List.head model.layers))


createTraceToHighlightNets : List (ConstructionPoint Thickness) -> { a | highlightNets : List Net } -> { a | highlightNets : List Net }
createTraceToHighlightNets points model =
    { model | highlightNets = List.map conductorNet (constructionPointsToConductors points) }


incrementNextNetId : { a | nextNetId : Int } -> { a | nextNetId : Int }
incrementNextNetId model =
    { model | nextNetId = model.nextNetId + 1 }
