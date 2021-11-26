module Visual exposing (..)

import Common exposing (Dimension(..), Point, Radius, Thickness, Width, fromPoint, toPairs)
import Conductor exposing (Conductor(..), ConstructionPoint(..), Net(..), SurfaceConductor(..), ThroughConductor(..), TracePoint, conductorNet, constructionPointA, constructionPointPoint, surfaceConductorNet)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Svg.Lazy


type VisualElement
    = Circle Conductor Point Radius
    | ConstructionCircle Point Radius (Maybe String)
    | Square Conductor Point Width (Maybe String)
    | SquareOutline Conductor Point Width (Maybe String)
    | ConstructionSquare Point Width (Maybe String)
    | Line Conductor Point Point Thickness
    | DashedLine Conductor Point Point Thickness
    | ConstructionLine Point Point Thickness
    | ConstructionSegment (List (ConstructionPoint Thickness))
    | ConstructionCrosshair (ConstructionPoint Thickness)


type Msg
    = Click VisualElement


elementConductor : VisualElement -> Maybe Conductor
elementConductor element =
    case element of
        Circle conductor _ _ ->
            Just conductor

        ConstructionCircle _ _ _ ->
            Nothing

        Square conductor _ _ _ ->
            Just conductor

        SquareOutline conductor _ _ _ ->
            Just conductor

        ConstructionSquare _ _ _ ->
            Nothing

        Line conductor _ _ _ ->
            Just conductor

        DashedLine conductor _ _ _ ->
            Just conductor

        ConstructionLine _ _ _ ->
            Nothing

        ConstructionSegment _ ->
            Nothing

        ConstructionCrosshair _ ->
            Nothing


viewVisualElement : ModelVisuals a -> VisualElement -> Svg Msg
viewVisualElement model element =
    case element of
        Circle _ point radius ->
            viewCircle point
                radius
                [ SvgA.fill (deriveColor model element)
                , SvgE.onClick (Click element)
                , SvgA.class "clickable"
                ]
                Nothing

        ConstructionCircle point radius maybeText ->
            let
                color =
                    deriveColor model element
            in
            viewCircle point
                radius
                [ SvgA.stroke color
                , SvgA.fill "none"
                ]
                (Maybe.map (\t -> ( t, color )) maybeText)

        Square _ point width maybeText ->
            viewSquare point
                width
                [ SvgA.fill <| deriveColor model element
                ]
                Nothing

        SquareOutline _ point width maybeText ->
            viewSquare point
                width
                [ SvgA.stroke <| deriveColor model element
                , SvgA.strokeWidth "2px"
                , SvgA.fill "none"
                ]
                Nothing

        ConstructionSquare point width maybeText ->
            let
                color =
                    deriveColor model element
            in
            viewSquare point
                width
                [ SvgA.stroke <| color
                , SvgA.strokeWidth "2px"
                , SvgA.fill "none"
                ]
                (Maybe.map (\t -> ( t, color )) maybeText)

        Line _ p1 p2 thickness ->
            viewLine p1
                p2
                thickness
                [ SvgA.stroke <| deriveColor model element
                , SvgE.onClick (Click element)
                , SvgA.strokeLinecap "round"
                , SvgA.class "clickable"
                ]

        DashedLine _ p1 p2 thickness ->
            viewLine p1
                p2
                thickness
                [ SvgA.stroke <| deriveColor model element
                , SvgE.onClick (Click element)
                , SvgA.class "clickable"
                , SvgA.strokeDasharray <| String.join "," [ String.fromFloat (2 * thickness), String.fromFloat (1 * thickness) ]
                ]

        ConstructionLine p1 p2 thickness ->
            viewLine p1
                p2
                thickness
                [ SvgA.stroke <| deriveColor model element
                ]

        ConstructionSegment cps ->
            Svg.g [] <|
                List.map (\( cp1, cp2 ) -> viewVisualElement model (ConstructionLine (constructionPointPoint cp1) (constructionPointPoint cp2) (constructionPointA cp2))) (toPairs cps)

        ConstructionCrosshair cp ->
            let
                length =
                    35

                ( center, color ) =
                    case cp of
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


viewCircle : Point -> Radius -> List (Svg.Attribute Msg) -> Maybe ( String, String ) -> Svg Msg
viewCircle point radius attrs maybeText =
    let
        textElement =
            case maybeText of
                Just ( text, color ) ->
                    [ Svg.text_
                        [ SvgA.x <| String.fromFloat point.x
                        , SvgA.y <| String.fromFloat point.y
                        , SvgA.dominantBaseline "middle"
                        , SvgA.textAnchor "middle"
                        , SvgA.pointerEvents "none"
                        , SvgA.fontSize <| String.fromFloat (radius * 2) ++ "px"
                        , SvgA.fill color
                        ]
                        [ Svg.text text ]
                    ]

                Nothing ->
                    []
    in
    Svg.g [] <|
        [ Svg.circle
            ([ SvgA.cx <| String.fromFloat point.x
             , SvgA.cy <| String.fromFloat point.y
             , SvgA.r <| String.fromFloat radius
             ]
                ++ attrs
            )
            []
        ]
            ++ textElement


viewSquare : Point -> Width -> List (Svg.Attribute Msg) -> Maybe ( String, String ) -> Svg Msg
viewSquare point width attrs maybeText =
    let
        half =
            width / 2

        textElement =
            case maybeText of
                Just ( text, color ) ->
                    [ Svg.text_
                        [ SvgA.x <| String.fromFloat point.x
                        , SvgA.y <| String.fromFloat point.y
                        , SvgA.dominantBaseline "middle"
                        , SvgA.textAnchor "middle"
                        , SvgA.pointerEvents "none"
                        , SvgA.fontSize <| String.fromFloat width ++ "px"
                        , SvgA.fill color
                        ]
                        [ Svg.text text ]
                    ]

                Nothing ->
                    []
    in
    Svg.g [] <|
        [ Svg.rect
            ([ SvgA.x <| String.fromFloat (point.x - half)
             , SvgA.y <| String.fromFloat (point.y - half)
             , SvgA.width <| String.fromFloat width
             , SvgA.height <| String.fromFloat width
             ]
                ++ attrs
            )
            []
        ]
            ++ textElement


viewLine p1 p2 thickness attrs =
    let
        d =
            SvgA.d <|
                String.join " " <|
                    [ fromPoint "M" p1
                    , fromPoint "L" p2
                    ]
    in
    Svg.path
        ([ d
         , SvgA.fill "none"
         , SvgA.strokeWidth (String.fromFloat thickness)
         ]
            ++ attrs
        )
        []


conductorToVisualElement : Conductor -> List VisualElement
conductorToVisualElement conductor =
    case conductor of
        Surface surfaceConductor ->
            surfaceConductorToVisualElement False surfaceConductor

        Through throughConductor ->
            throughConductorToVisualElement throughConductor


throughConductorToVisualElement : ThroughConductor -> List VisualElement
throughConductorToVisualElement throughConductor =
    case throughConductor of
        ThroughPad pad point radius _ ->
            -- todo pad
            [ Circle (Through throughConductor) point radius ]


viewLazyThroughConductors : ModelVisuals a -> List ThroughConductor -> Svg Msg
viewLazyThroughConductors appearance throughConductors =
    Svg.Lazy.lazy3
        (\highlightNets select tc ->
            viewThroughConductors { highlightNets = highlightNets, select = select } tc
        )
        appearance.highlightNets
        appearance.select
        throughConductors


viewThroughConductors : ModelVisuals a -> List ThroughConductor -> Svg Msg
viewThroughConductors appearance throughConductors =
    Svg.g []
        (List.concatMap throughConductorToVisualElement throughConductors
            |> List.map (viewVisualElement appearance)
        )


surfaceConductorToVisualElement : Bool -> SurfaceConductor -> List VisualElement
surfaceConductorToVisualElement hidden surfaceConductor =
    let
        c =
            Surface surfaceConductor
    in
    case ( hidden, surfaceConductor ) of
        ( _, Trace tracePoints _ ) ->
            traceToVisualElements hidden c tracePoints

        -- todo
        ( False, SurfacePad pad point width _ ) ->
            -- todo pad
            [ Square c point width (Maybe.map String.fromInt pad.number) ]

        ( True, SurfacePad pad point width _ ) ->
            -- todo pad
            [ SquareOutline c point width (Maybe.map String.fromInt pad.number) ]

        ( _, Zone points net ) ->
            -- TODO
            []


traceToVisualElements : Bool -> Conductor -> List TracePoint -> List VisualElement
traceToVisualElements hidden c tracePoints =
    let
        element =
            if hidden then
                DashedLine

            else
                Line
    in
    case tracePoints of
        p1 :: p2 :: rest ->
            element c p1.point p2.point p2.thickness :: traceToVisualElements hidden c (p2 :: rest)

        _ ->
            []


type alias ModelVisuals a =
    { a | highlightNets : List Net, select : List VisualElement }


deriveColor : ModelVisuals a -> VisualElement -> String
deriveColor model element =
    let
        net =
            elementConductor element
                |> Maybe.map conductorNet
                |> Maybe.withDefault (NoNet 0)
    in
    if List.member element model.select then
        case net of
            NoNet _ ->
                "cyan"

            AutoNet _ ->
                "cyan"

            CustomNet _ c ->
                "cyan"

    else if List.member net model.highlightNets then
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


viewSurfaceConductors : ModelVisuals a -> List { b | conductors : List SurfaceConductor } -> Svg Msg
viewSurfaceConductors model layers =
    case layers of
        layer :: hiddenLayers ->
            let
                highlightedHiddenSurfaceConductors =
                    List.concatMap .conductors hiddenLayers
                        |> List.filter (\c -> List.member (surfaceConductorNet c) model.highlightNets)

                f hidden =
                    List.map (viewVisualElement model) << surfaceConductorToVisualElement hidden
            in
            Svg.Lazy.lazy2
                (\c cHidden ->
                    Svg.g [] <|
                        List.concatMap (f False) c
                            ++ List.concatMap (f True) cHidden
                )
                layer.conductors
                highlightedHiddenSurfaceConductors

        [] ->
            Svg.text ""


viewDimensions : ModelVisuals a -> List Dimension -> Svg Msg
viewDimensions model dimensions =
    Svg.g [] (List.map (viewDimension model) dimensions)


viewDimension : ModelVisuals a -> Dimension -> Svg Msg
viewDimension model dimension =
    case dimension of
        DistanceDimension p1 p2 ->
            Svg.g []
                [ viewVisualElement model (ConstructionCircle p1 5 Nothing)
                , viewVisualElement model (ConstructionLine p1 p2 1)
                , viewVisualElement model (ConstructionCircle p2 5 Nothing)
                ]

        AngleDimension p1 p2 p3 ->
            Svg.g []
                [ viewVisualElement model (ConstructionCircle p1 5 Nothing)
                , viewVisualElement model (ConstructionCircle p2 5 Nothing)
                , viewVisualElement model (ConstructionCircle p3 5 Nothing)
                , viewVisualElement model (ConstructionLine p1 p2 1)
                , viewVisualElement model (ConstructionLine p1 p3 1)
                ]
