module Conductor exposing (..)

import Common exposing (Pad, Point, Radius, Thickness, ThreePoints, TwoPoints, Width, distanceToPoint, unique)


type Interaction
    = NoInteraction
    | PointInteraction (List Conductor) Point
    | SegmentInteraction Conductor Point Point
    | NetInteraction Net


type alias Selection =
    Interaction


type alias Highlight =
    Interaction


type alias InteractionInformation =
    ( Selection, Highlight )


isConductorInPrimaryInteraction : Conductor -> Interaction -> Bool
isConductorInPrimaryInteraction conductor interaction =
    case interaction of
        NoInteraction ->
            False

        PointInteraction conductors _ ->
            List.member conductor conductors

        SegmentInteraction traceConductor _ _ ->
            conductor == traceConductor

        NetInteraction net ->
            conductorNet conductor == net


isConductorInSecondaryInteraction : Conductor -> Interaction -> Bool
isConductorInSecondaryInteraction conductor interaction =
    let
        net =
            conductorNet conductor
    in
    case interaction of
        NoInteraction ->
            False

        PointInteraction conductors _ ->
            List.member net (List.map conductorNet conductors)

        SegmentInteraction traceConductor _ _ ->
            net == conductorNet traceConductor

        NetInteraction _ ->
            -- NetInteraction is primary only
            False


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


isConductorInPoints : List Point -> Conductor -> Bool
isConductorInPoints points conductor =
    List.foldl
        (\p b ->
            if b then
                b

            else
                List.member p points
        )
        False
        (conductorPoints conductor)


{-| Given list of conductors that are known to be connected and a list of conductors that might be connected,
return a tuple containing list of conductors that are connected together and conductors that are not connected together.

In a normal use case, the connected argument should be a singleton when starting the iteratoin.

-}
findConnected : List Conductor -> List Conductor -> ( List Conductor, List Conductor )
findConnected connected unconnected =
    case connected of
        [] ->
            ( [], unconnected )

        initial :: remainingConnected ->
            let
                initialPoints =
                    conductorPoints initial

                ( newConnected, newUnconnected ) =
                    List.partition (isConductorInPoints initialPoints) unconnected

                ( recursiveConnected, recursiveUnconnected ) =
                    findConnected (remainingConnected ++ newConnected) newUnconnected
            in
            -- For every connected conductor, we redo the search with the remaining unconnected conductors
            ( initial :: recursiveConnected, recursiveUnconnected )


{-| splitNet function takes a net and checks it for net splits
-}
checkNetSplit : Net -> ModelConductors a b -> ModelConductors a b
checkNetSplit net model =
    let
        modelConductors =
            allConductors model

        netConductors =
            netsConductors modelConductors [ net ]
    in
    case modelConductors of
        [] ->
            model

        initial :: rest ->
            let
                ( connected, unconnected ) =
                    findConnected [ initial ] rest
            in
            case ( connected, unconnected ) of
                -- Edge case: If we have only one connected, might as well split everything
                ( [ c ], ucs ) ->
                    splitNet model (c :: ucs)

                ( _, [] ) ->
                    model

                ( _, ucs ) ->
                    splitNet model ucs


{-| Split the given conductors from their existing net into new net(s)
-}
splitNet : ModelConductors a b -> List Conductor -> ModelConductors a b
splitNet model conductors =
    -- TODO deal with custom net !
    case conductors of
        [] ->
            model

        first :: rest ->
            let
                net =
                    conductorNet first

                ( connected, unconnected ) =
                    findConnected [ first ] rest
            in
            (case connected of
                [] ->
                    model

                [ c ] ->
                    setConductorsNet model [ c ] (NoNet model.nextNetId)
                        |> (\m -> { m | nextNetId = m.nextNetId + 1 })

                cs ->
                    case net of
                        NoNet _ ->
                            setConductorsNet model cs (NoNet model.nextNetId)
                                |> (\m -> { m | nextNetId = m.nextNetId + 1 })

                        AutoNet _ ->
                            setConductorsNet model cs (AutoNet model.nextNetId)
                                |> (\m -> { m | nextNetId = m.nextNetId + 1 })

                        CustomNet _ _ ->
                            -- TODO !
                            model
            )
                |> (\m -> splitNet m unconnected)


setConductorsNet : ModelConductors a b -> List Conductor -> Net -> ModelConductors a b
setConductorsNet model conductors net =
    List.foldl (updateConductorNet net) model conductors


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


mapConstructionPoint : (a -> b) -> ConstructionPoint a -> ConstructionPoint b
mapConstructionPoint f cp =
    case cp of
        FreePoint p a ->
            FreePoint p (f a)

        SnapPoint p c a ->
            SnapPoint p c (f a)


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


removeConductor : Conductor -> ModelConductors a b -> ModelConductors a b
removeConductor conductor model =
    case conductor of
        Through c ->
            removeThroughConductor model c

        Surface c ->
            removeSurfaceConductor model c


removeThroughConductor : ModelConductors a b -> ThroughConductor -> ModelConductors a b
removeThroughConductor model tc =
    { model | conductors = List.filter (\x -> x /= tc) model.conductors }


removeSurfaceConductor : ModelConductors a b -> SurfaceConductor -> ModelConductors a b
removeSurfaceConductor model sc =
    { model
        | layers =
            List.map
                (\l ->
                    { l | conductors = List.filter (\x -> x /= sc) l.conductors }
                )
                model.layers
    }


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



--createTraceToHighlightNets : List (ConstructionPoint Thickness) -> { a | highlightNets : List Net } -> { a | highlightNets : List Net }
--createTraceToHighlightNets points model =
--    { model | highlightNets = List.map conductorNet (constructionPointsToConductors points) }


incrementNextNetId : { a | nextNetId : Int } -> { a | nextNetId : Int }
incrementNextNetId model =
    { model | nextNetId = model.nextNetId + 1 }
