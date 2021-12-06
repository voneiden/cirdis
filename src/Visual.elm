module Visual exposing (..)

import Common exposing (Dimension(..), Point, Radius, ReferenceFrame, Thickness, Width, fromPoint, toPairs)
import Conductor exposing (Conductor(..), ConstructionPoint(..), Highlight, InteractionInformation, Net(..), Selection, SurfaceConductor(..), ThroughConductor(..), Tool, TracePoint, conductorNet, constructionPointA, constructionPointPoint, interactionInformation, isConductorInteraction, surfaceConductorNet)
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Svg.Lazy
import Vector


type VisualElement
    = Circle Conductor Point Radius (Maybe String)
    | ConstructionCircle Point Radius (Maybe String)
    | Square Conductor Point Width (Maybe String)
    | SquareOutline Conductor Point Width (Maybe String)
    | ConstructionSquare Point Width (Maybe String)
    | Line Conductor Point Point Thickness
    | DashedLine Conductor Point Point Thickness
    | ConstructionLine Point Point Thickness
    | ConstructionSegment (List (ConstructionPoint Thickness))
    | ConstructionCrosshair (ConstructionPoint Thickness)
    | Text Point String Int Float
    | Background


type Msg
    = Click VisualElement
    | MouseOver VisualElement
    | MouseOut VisualElement


elementConductor : VisualElement -> Maybe Conductor
elementConductor element =
    case element of
        Circle conductor _ _ _ ->
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

        Text _ _ _ _ ->
            Nothing

        Background ->
            Nothing


viewVisualElement : ModelVisuals a -> VisualElement -> Svg Msg
viewVisualElement model element =
    case element of
        Circle _ point radius maybeText ->
            let
                color =
                    deriveColor model element
            in
            viewCircle point
                radius
                [ SvgA.fill color
                , SvgE.stopPropagationOn "click" (Decode.succeed ( Click element, True ))
                , SvgE.onMouseOver (MouseOver element)
                , SvgE.onMouseOut (MouseOut element)
                , SvgA.class "clickable"
                ]
                (Maybe.map (\t -> ( t, "white" )) maybeText)

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
                (Maybe.map (\t -> ( t, "white" )) maybeText)

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
                , SvgE.stopPropagationOn "click" (Decode.succeed ( Click element, True ))
                , SvgE.onMouseOver (MouseOver element)
                , SvgE.onMouseOut (MouseOut element)
                , SvgA.strokeLinecap "round"
                , SvgA.class "clickable"
                ]

        DashedLine _ p1 p2 thickness ->
            viewLine p1
                p2
                thickness
                [ SvgA.stroke <| deriveColor model element
                , SvgE.stopPropagationOn "click" (Decode.succeed ( Click element, True ))
                , SvgE.onMouseOver (MouseOver element)
                , SvgE.onMouseOut (MouseOut element)
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
                List.map (\( cp1, cp2 ) -> viewVisualElement model interaction (ConstructionLine (constructionPointPoint cp1) (constructionPointPoint cp2) (constructionPointA cp2))) (toPairs cps)

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

        Text point string size rotation ->
            Svg.text_
                [ SvgA.x <| String.fromFloat point.x
                , SvgA.y <| String.fromFloat point.y
                , SvgA.dominantBaseline "middle"
                , SvgA.textAnchor "middle"
                , SvgA.pointerEvents "none"
                , SvgA.fontSize <| String.fromInt size ++ "px"
                , SvgA.fill "red"
                , SvgA.style <| "transform-box: fill-box;transform-origin: center;transform:rotate(" ++ String.fromFloat rotation ++ "rad)"
                ]
                [ Svg.text string ]

        Background ->
            Svg.text ""


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
                        , SvgA.fontSize <| String.fromFloat (radius * 1.5) ++ "px"
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
                        , SvgA.fontSize <| String.fromFloat (width * 0.75) ++ "px"
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
            [ Circle (Through throughConductor) point radius (Maybe.map String.fromInt pad.number) ]


viewLazyThroughConductors : ModelVisuals a -> InteractionInformation -> List ThroughConductor -> Svg Msg
viewLazyThroughConductors appearance interaction throughConductors =
    Svg.Lazy.lazy3
        (\_ _ tc ->
            viewThroughConductors appearance interaction tc
        )
        appearance.ref
        interaction
        throughConductors


viewThroughConductors : ModelVisuals a -> InteractionInformation -> List ThroughConductor -> Svg Msg
viewThroughConductors appearance interaction throughConductors =
    Svg.g []
        (List.concatMap throughConductorToVisualElement throughConductors
            |> List.map (viewVisualElement appearance interaction)
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
    { a
        | ref : Maybe ReferenceFrame
        , tool : Tool
    }


toolSelection : ModelVisuals a -> Selection
toolSelection model =
    Tuple.first <| interactionInformation model.tool

toolHighlight : ModelVisuals a -> Highlight
toolHighlight model =
    Tuple.second <| interactionInformation model.tool

isMaybeConductorSelected : ModelVisuals a -> Maybe Conductor -> Bool
isMaybeConductorSelected model mc =
    case mc of
        Just c ->
            isConductorInteraction c (toolSelection model)

        Nothing ->
            False


isMaybeConductorHighlighted : ModelVisuals a -> Maybe Conductor -> Bool
isMaybeConductorHighlighted model mc =
    case mc of
        Just c ->
            isConductorInteraction c (toolHighlight model)

        Nothing ->
            False


deriveColor : ModelVisuals a -> VisualElement -> String
deriveColor model element =
    let
        mc =
            elementConductor element

        net =
            mc
                |> Maybe.map conductorNet
                |> Maybe.withDefault (NoNet 0)
    in
    if isMaybeConductorSelected model mc then
        case net of
            NoNet _ ->
                "cyan"

            AutoNet _ ->
                "cyan"

            CustomNet _ c ->
                "cyan"

    else if isMaybeConductorHighlighted model mc then
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


viewSurfaceConductors : ModelVisuals a -> InteractionInformation -> List { b | conductors : List SurfaceConductor } -> Svg Msg
viewSurfaceConductors model interaction layers =
    case layers of
        layer :: hiddenLayers ->
            let
                highlightedHiddenSurfaceConductors =
                    List.concatMap .conductors hiddenLayers
                        |> List.filter (\c -> List.member (surfaceConductorNet c) model.highlightNets)

                f hidden =
                    List.map (viewVisualElement model interaction) << surfaceConductorToVisualElement hidden
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


viewDimensions : ModelVisuals a -> InteractionInformation -> List Dimension -> Svg Msg
viewDimensions model interaction dimensions =
    Svg.g [] (List.map (viewDimension model interaction) dimensions)


viewDimension : ModelVisuals a -> InteractionInformation -> Dimension -> Svg Msg
viewDimension model interaction dimension =
    case dimension of
        DistanceDimension p1 p2 ->
            let
                v =
                    Vector.sub p2 p1

                l =
                    Vector.len v

                u =
                    Vector.unit v l

                middle =
                    Vector.mul u (l / 2)

                offset =
                    Vector.mul (Vector.rot90ccw u) 30

                textRotation =
                    atan2 v.y v.x

                textPosition =
                    Vector.sum p1 (Vector.sum middle offset)

                referenceLength =
                    case model.ref of
                        Just ref ->
                            let
                                refLen =
                                    l * ref.ratio

                                roughlyRounded =
                                    (refLen
                                        * 100
                                        |> round
                                        |> toFloat
                                    )
                                        / 100
                            in
                            (String.fromFloat <| roughlyRounded) ++ " " ++ ref.unit

                        Nothing ->
                            "?"
            in
            Svg.g []
                [ viewVisualElement model interaction (ConstructionCircle p1 5 Nothing)
                , viewVisualElement model interaction (ConstructionLine p1 p2 1)
                , viewVisualElement model interaction (ConstructionCircle p2 5 Nothing)
                , viewVisualElement model interaction (Text textPosition referenceLength 20 textRotation)
                ]

        AngleDimension p1 p2 p3 ->
            Svg.g []
                [ viewVisualElement model interaction (ConstructionCircle p1 5 Nothing)
                , viewVisualElement model interaction (ConstructionCircle p2 5 Nothing)
                , viewVisualElement model interaction (ConstructionCircle p3 5 Nothing)
                , viewVisualElement model interaction (ConstructionLine p1 p2 1)
                , viewVisualElement model interaction (ConstructionLine p1 p3 1)
                ]
