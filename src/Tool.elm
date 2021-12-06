module Tool exposing (..)

import Common exposing (Dimension(..), Point, Radius, ReferenceFrame, Thickness, ThreePoints(..), TwoPoints(..), chainUpdate3)
import Conductor
    exposing
        ( Conductor
        , ConstructionPoint(..)
        , MergeNet(..)
        , ModelConductors
        , Net(..)
        , SurfaceConductor(..)
        , ThroughConductor(..)
        , activeLayerSurfaceConductors
        , addSurfaceConductor
        , addSurfaceConductorNoNet
        , addThroughConductor
        , constructionPointPoint
        , constructionPointsToConductors
        , constructionPointsToTrace
        , createTraceToHighlightNets
        , incrementNextNetId
        , mergeNets
        , snapTo
        , updateConductorNet
        )
import Form
import Svg exposing (Svg)
import Vector exposing (generateDoubleRow, generateSingleRow)
import Visual exposing (ModelVisuals, VisualElement(..), viewVisualElement)



-- MODEL


type Tool
    = SelectTool Selection Highlight
    | CreateTraceTool (List (ConstructionPoint Thickness)) Highlight
    | CreateSurfacePadTool
    | CreateNumberedSurfacePad Int
    | CreateSoicSurfacePad ThreePoints
    | CreateRowSurfacePad Int TwoPoints -- TODO row should support setting starting pin number
    | CreateThroughPadTool
    | CreateNumberedThroughPad Int
    | CreateDipThroughPad ThreePoints
    | CreateRowThroughPad Int TwoPoints
    | CreateZoneTool
    | DefineReferenceFrame (Maybe Point) (Maybe Point)
    | CreateDistanceDimension (Maybe Point)
    | CreateAngleDimension (Maybe Point) (Maybe Point)


type Interaction
    = NoInteraction
    | PointInteraction (List Conductor) Point
    | SegmentInteraction Conductor Point Point
    | NetInteraction Net


type alias Selection =
    Interaction


type alias Highlight =
    Interaction


resetTool : Tool -> Tool
resetTool tool =
    case tool of
        SelectTool _ _ ->
            SelectTool NoInteraction NoInteraction

        CreateTraceTool _ _ ->
            CreateTraceTool [] NoInteraction

        CreateSurfacePadTool ->
            CreateSurfacePadTool

        CreateThroughPadTool ->
            CreateThroughPadTool

        CreateZoneTool ->
            CreateZoneTool

        CreateNumberedSurfacePad _ ->
            CreateNumberedSurfacePad 1

        CreateSoicSurfacePad _ ->
            CreateSoicSurfacePad NoneOfThree

        CreateRowSurfacePad _ _ ->
            CreateRowSurfacePad 1 NoneOfTwo

        CreateNumberedThroughPad _ ->
            CreateNumberedThroughPad 1

        CreateDipThroughPad _ ->
            CreateDipThroughPad NoneOfThree

        CreateRowThroughPad _ _ ->
            CreateRowThroughPad 1 NoneOfTwo

        DefineReferenceFrame _ _ ->
            DefineReferenceFrame Nothing Nothing

        CreateDistanceDimension _ ->
            CreateDistanceDimension Nothing

        CreateAngleDimension _ _ ->
            CreateAngleDimension Nothing Nothing


selectionHighlight : Tool -> ( Selection, Highlight )
selectionHighlight tool =
    case tool of
        SelectTool selection highlight ->
            ( selection, highlight )

        CreateTraceTool _ highlight ->
            ( NoInteraction, highlight )

        _ ->
            ( NoInteraction, NoInteraction )


toolToString : Tool -> String
toolToString tool =
    case tool of
        SelectTool _ _ ->
            "select"

        CreateTraceTool _ _ ->
            "trace"

        CreateSurfacePadTool ->
            "surface"

        CreateThroughPadTool ->
            "through"

        CreateZoneTool ->
            "zone"

        CreateNumberedSurfacePad _ ->
            "numbered-surface"

        CreateSoicSurfacePad _ ->
            "soic-surface"

        CreateRowSurfacePad _ _ ->
            "row-surface"

        CreateNumberedThroughPad _ ->
            "numbered-thorugh"

        CreateDipThroughPad _ ->
            "dip-through"

        CreateRowThroughPad _ _ ->
            "row-through"

        DefineReferenceFrame _ _ ->
            "reference-frame"

        CreateDistanceDimension _ ->
            "distance-dimension"

        CreateAngleDimension _ _ ->
            "angle-dimension"


setToolSelection : Tool -> Selection -> Tool
setToolSelection tool selection =
    case tool of
        SelectTool _ highlight ->
            SelectTool selection highlight

        _ ->
            tool


setToolHighlight : Tool -> Highlight -> Tool
setToolHighlight tool highlight =
    case tool of
        SelectTool selection _ ->
            SelectTool selection highlight

        CreateTraceTool cps _ ->
            CreateTraceTool cps highlight

        _ ->
            tool



-- UPDATE


type Msg
    = LeftClick Point
    | ShiftScroll Float
    | CtrlScroll Float
    | SetTool Tool
    | Reset
    | SetSubTool Int
    | FormMsg Form.Msg


type alias ModelTools a =
    { a
        | tool : Tool
        , thickness : Thickness
        , radius : Radius
        , cursor : Point
        , snapDistance : Float
        , ref : Maybe ReferenceFrame
        , form : Form.Form
        , dimensions : List Dimension
    }


update : (Msg -> msg) -> Msg -> ModelTools (ModelVisuals (ModelConductors a b)) -> ( ModelTools (ModelVisuals (ModelConductors a b)), Cmd msg, Bool )
update toMsg msg model =
    case msg of
        LeftClick point ->
            let
                snapPoint =
                    snapTo model.snapDistance point model.conductors (activeLayerSurfaceConductors model)
            in
            case model.tool of
                CreateTraceTool points highlight ->
                    case snapPoint model.thickness of
                        SnapPoint p c t ->
                            let
                                newPoints =
                                    points ++ [ SnapPoint p c t ]
                            in
                            if List.isEmpty points then
                                ( { model | tool = CreateTraceTool newPoints highlight }
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
                                            |> resetModelTool
                                            |> createTraceToHighlightNets newPoints
                                        , Cmd.none
                                        , True
                                        )

                                    MergeConflict nets conductors ->
                                        -- TODO show somekind of conflict resolution thing
                                        ( resetModelTool model, Cmd.none, True )

                                    MergeNoNet conductors ->
                                        let
                                            net =
                                                AutoNet model.nextNetId
                                        in
                                        ( List.foldl (updateConductorNet net) model conductors
                                            |> addSurfaceConductor (constructionPointsToTrace newPoints net)
                                            |> resetModelTool
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
                                ( { model | tool = CreateTraceTool t2 highlight }, Cmd.none, False )

                CreateThroughPadTool ->
                    ( addThroughConductor (ThroughPad { number = Nothing, label = Nothing } point model.radius) model, Cmd.none, True )

                CreateNumberedThroughPad pinNumber ->
                    ( { model | tool = CreateNumberedThroughPad (pinNumber + 1) }
                        |> addThroughConductor (ThroughPad { number = Just pinNumber, label = Nothing } point model.radius)
                    , Cmd.none
                    , True
                    )

                CreateDipThroughPad someOfThree ->
                    case someOfThree of
                        NoneOfThree ->
                            ( { model | tool = CreateDipThroughPad (OneOfThree point) }, Cmd.none, False )

                        OneOfThree p1 ->
                            ( { model | tool = CreateDipThroughPad (TwoOfThree p1 point) }, Cmd.none, False )

                        TwoOfThree p1 p2 ->
                            ( { model | tool = CreateDipThroughPad (ThreeOfThree p1 p2 point) }, Cmd.none, False )

                        ThreeOfThree p1 p2 p3 ->
                            let
                                shape =
                                    generateDoubleRow (model.radius * 2) p1 p2 p3 model.cursor
                            in
                            ( List.foldl
                                (\( shapePoint, shapePad ) m ->
                                    addThroughConductor (ThroughPad shapePad shapePoint model.radius) m
                                )
                                { model | tool = CreateDipThroughPad NoneOfThree }
                                shape
                            , Cmd.none
                            , True
                            )

                CreateRowThroughPad startNumber someOfTwo ->
                    case someOfTwo of
                        NoneOfTwo ->
                            ( { model | tool = CreateRowThroughPad startNumber (OneOfTwo point) }, Cmd.none, False )

                        OneOfTwo p1 ->
                            ( { model | tool = CreateRowThroughPad startNumber (TwoOfTwo p1 point) }, Cmd.none, False )

                        TwoOfTwo p1 p2 ->
                            let
                                shape =
                                    generateSingleRow startNumber (model.radius * 2) p1 p2 model.cursor

                                lastPinNumber =
                                    shape
                                        |> List.reverse
                                        |> List.head
                                        |> Maybe.andThen (\( _, pad ) -> pad.number)
                                        |> Maybe.map (\number -> number + 1)
                                        |> Maybe.withDefault 1
                            in
                            ( List.foldl
                                (\( shapePoint, shapePad ) m ->
                                    addThroughConductor (ThroughPad shapePad shapePoint model.radius) m
                                )
                                { model | tool = CreateRowThroughPad lastPinNumber NoneOfTwo }
                                shape
                            , Cmd.none
                            , True
                            )

                CreateSurfacePadTool ->
                    ( addSurfaceConductorNoNet (SurfacePad { number = Nothing, label = Nothing } point (model.radius * 2)) model, Cmd.none, True )

                CreateNumberedSurfacePad pinNumber ->
                    ( { model | tool = CreateNumberedSurfacePad (pinNumber + 1) }
                        |> addSurfaceConductorNoNet (SurfacePad { number = Just pinNumber, label = Nothing } point (model.radius * 2))
                    , Cmd.none
                    , True
                    )

                CreateSoicSurfacePad someOfThree ->
                    case someOfThree of
                        NoneOfThree ->
                            ( { model | tool = CreateSoicSurfacePad (OneOfThree point) }, Cmd.none, False )

                        OneOfThree p1 ->
                            ( { model | tool = CreateSoicSurfacePad (TwoOfThree p1 point) }, Cmd.none, False )

                        TwoOfThree p1 p2 ->
                            ( { model | tool = CreateSoicSurfacePad (ThreeOfThree p1 p2 point) }, Cmd.none, False )

                        ThreeOfThree p1 p2 p3 ->
                            let
                                shape =
                                    generateDoubleRow (model.radius * 2) p1 p2 p3 model.cursor
                            in
                            ( List.foldl
                                (\( shapePoint, shapePad ) m ->
                                    addSurfaceConductorNoNet (SurfacePad shapePad shapePoint (model.radius * 2)) m
                                )
                                { model | tool = CreateSoicSurfacePad NoneOfThree }
                                shape
                            , Cmd.none
                            , True
                            )

                CreateRowSurfacePad startNumber someOfTwo ->
                    case someOfTwo of
                        NoneOfTwo ->
                            ( { model | tool = CreateRowSurfacePad startNumber (OneOfTwo point) }, Cmd.none, False )

                        OneOfTwo p1 ->
                            ( { model | tool = CreateRowSurfacePad startNumber (TwoOfTwo p1 point) }, Cmd.none, False )

                        TwoOfTwo p1 p2 ->
                            let
                                shape =
                                    generateSingleRow startNumber (model.radius * 2) p1 p2 model.cursor

                                lastPinNumber =
                                    shape
                                        |> List.reverse
                                        |> List.head
                                        |> Maybe.andThen (\( _, pad ) -> pad.number)
                                        |> Maybe.map (\number -> number + 1)
                                        |> Maybe.withDefault 1
                            in
                            ( List.foldl
                                (\( shapePoint, shapePad ) m ->
                                    addSurfaceConductorNoNet (SurfacePad shapePad shapePoint (model.radius * 2)) m
                                )
                                { model | tool = CreateRowSurfacePad lastPinNumber NoneOfTwo }
                                shape
                            , Cmd.none
                            , True
                            )

                DefineReferenceFrame mp1 mp2 ->
                    case ( model.ref, mp1, mp2 ) of
                        ( Nothing, Nothing, Nothing ) ->
                            ( { model | tool = DefineReferenceFrame (Just point) Nothing }, Cmd.none, False )

                        ( Nothing, Just p1, Nothing ) ->
                            ( { model | tool = DefineReferenceFrame (Just p1) (Just point) }, Cmd.none, False )
                                |> chainUpdate3 (\m -> Form.update (toMsg << FormMsg) (Form.RefMsg <| Form.RefInit model.ref p1 point) m)

                        _ ->
                            ( model, Cmd.none, False )

                CreateDistanceDimension mp1 ->
                    case mp1 of
                        Nothing ->
                            ( { model | tool = CreateDistanceDimension (Just point) }, Cmd.none, False )

                        Just p1 ->
                            ( { model
                                | tool = CreateDistanceDimension Nothing
                                , dimensions = DistanceDimension p1 point :: model.dimensions
                              }
                            , Cmd.none
                            , False
                            )

                CreateAngleDimension mp1 mp2 ->
                    case ( mp1, mp2 ) of
                        ( Just _, Nothing ) ->
                            ( { model | tool = CreateAngleDimension mp1 (Just point) }, Cmd.none, False )

                        ( Just p1, Just p2 ) ->
                            ( { model
                                | tool = CreateAngleDimension Nothing Nothing
                                , dimensions = AngleDimension p1 p2 point :: model.dimensions
                              }
                            , Cmd.none
                            , False
                            )

                        _ ->
                            ( { model | tool = CreateAngleDimension (Just point) Nothing }, Cmd.none, False )

                _ ->
                    ( model, Cmd.none, False )

        ShiftScroll delta ->
            case model.tool of
                CreateThroughPadTool ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateNumberedThroughPad _ ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateDipThroughPad _ ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateRowThroughPad _ _ ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateSurfacePadTool ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateNumberedSurfacePad _ ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateSoicSurfacePad _ ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateRowSurfacePad _ _ ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateTraceTool _ _ ->
                    ( updateThickness delta model, Cmd.none, False )

                _ ->
                    ( model, Cmd.none, False )

        CtrlScroll delta ->
            let
                step =
                    if delta > 0 then
                        -1

                    else if delta < 0 then
                        1

                    else
                        0
            in
            case model.tool of
                CreateNumberedSurfacePad startNumber ->
                    ( { model | tool = CreateNumberedSurfacePad (max 1 (startNumber + step)) }, Cmd.none, False )

                CreateRowSurfacePad startNumber someOfTwo ->
                    ( { model | tool = CreateRowSurfacePad (max 1 (startNumber + step)) someOfTwo }, Cmd.none, False )

                CreateNumberedThroughPad startNumber ->
                    ( { model | tool = CreateNumberedThroughPad (max 1 (startNumber + step)) }, Cmd.none, False )

                CreateRowThroughPad startNumber someOfTwo ->
                    ( { model | tool = CreateRowThroughPad (max 1 (startNumber + step)) someOfTwo }, Cmd.none, False )

                _ ->
                    ( model, Cmd.none, False )

        SetTool tool ->
            let
                checkRef t =
                    case model.ref of
                        Nothing ->
                            ( { model | tool = DefineReferenceFrame Nothing Nothing }, Cmd.none, False )

                        Just ref ->
                            ( { model | tool = t }, Cmd.none, False )
            in
            case ( tool, model.tool ) of
                ( CreateTraceTool [] NoInteraction, CreateTraceTool trace selection ) ->
                    ( { model
                        | tool =
                            CreateTraceTool
                                (trace
                                    |> List.reverse
                                    |> List.drop 1
                                    |> List.reverse
                                )
                                selection
                      }
                    , Cmd.none
                    , False
                    )

                ( CreateDistanceDimension _, _ ) ->
                    checkRef tool

                ( CreateAngleDimension _ _, _ ) ->
                    checkRef tool

                ( DefineReferenceFrame _ _, _ ) ->
                    case model.ref of
                        Just ref ->
                            ( { model | tool = DefineReferenceFrame (Just ref.p1) (Just ref.p2) }, Cmd.none, False )
                                |> chainUpdate3 (\m -> Form.update (toMsg << FormMsg) (Form.RefMsg <| Form.RefInit model.ref ref.p1 ref.p2) m)

                        Nothing ->
                            ( { model | tool = tool }, Cmd.none, False )

                _ ->
                    ( { model | tool = tool, highlightNets = [] }, Cmd.none, False )

        Reset ->
            ( resetModelTool model, Cmd.none, False )

        SetSubTool index ->
            case model.tool of
                SelectTool _ _ ->
                    ( model, Cmd.none, False )

                CreateTraceTool _ _ ->
                    ( model, Cmd.none, False )

                CreateSurfacePadTool ->
                    update toMsg (SetTool (indexToSurfacePadTool index)) model

                CreateNumberedSurfacePad _ ->
                    update toMsg (SetTool (indexToSurfacePadTool index)) model

                CreateSoicSurfacePad _ ->
                    update toMsg (SetTool (indexToSurfacePadTool index)) model

                CreateRowSurfacePad _ _ ->
                    update toMsg (SetTool (indexToSurfacePadTool index)) model

                CreateThroughPadTool ->
                    update toMsg (SetTool (indexToThroughPadTool index)) model

                CreateNumberedThroughPad _ ->
                    update toMsg (SetTool (indexToThroughPadTool index)) model

                CreateDipThroughPad _ ->
                    update toMsg (SetTool (indexToThroughPadTool index)) model

                CreateRowThroughPad _ _ ->
                    update toMsg (SetTool (indexToThroughPadTool index)) model

                CreateZoneTool ->
                    ( model, Cmd.none, False )

                DefineReferenceFrame _ _ ->
                    update toMsg (SetTool (indexToDimensionTool model.tool index)) model

                CreateDistanceDimension _ ->
                    update toMsg (SetTool (indexToDimensionTool model.tool index)) model

                CreateAngleDimension _ _ ->
                    update toMsg (SetTool (indexToDimensionTool model.tool index)) model

        FormMsg formMsg ->
            Form.update (toMsg << FormMsg) formMsg model


resetModelTool : { a | tool : Tool } -> { a | tool : Tool }
resetModelTool model =
    { model | tool = resetTool model.tool }


updateRadius : Float -> { a | radius : Radius } -> { a | radius : Radius }
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


updateThickness : Float -> { a | thickness : Thickness } -> { a | thickness : Thickness }
updateThickness delta model =
    let
        multiplier =
            1.25

        newZ =
            if delta < 0 then
                model.thickness * multiplier

            else
                model.thickness / multiplier
    in
    if newZ > 0.9 && newZ < 1.1 then
        { model | thickness = 1 }

    else
        { model | thickness = newZ }



-- VIEWS


{-| Display anything and everything related to the active tool
-}
viewTool : ModelTools (ModelVisuals (ModelConductors a b)) -> Svg Visual.Msg
viewTool model =
    case model.tool of
        SelectTool selection highlight ->
            let
                cp =
                    snapTo model.snapDistance model.cursor model.conductors (activeLayerSurfaceConductors model) model.thickness
            in
            case selection of
                NoInteraction ->
                    viewVisualElement model (ConstructionCrosshair cp)

                PointInteraction conductors p1 ->
                    Svg.g []
                        [ viewVisualElement model (ConstructionCrosshair (FreePoint p1 0))
                        , viewVisualElement model (ConstructionCrosshair cp)
                        ]

                SegmentInteraction conductor p1 p2 ->
                    Svg.g []
                        [ viewVisualElement model (ConstructionCrosshair (FreePoint p1 0))
                        , viewVisualElement model (ConstructionCrosshair (FreePoint p2 0))
                        , viewVisualElement model (ConstructionCrosshair cp)
                        ]

                NetInteraction net ->
                    viewVisualElement model (ConstructionCrosshair cp)

        CreateTraceTool cps highlight ->
            let
                cp =
                    snapTo model.snapDistance model.cursor model.conductors (activeLayerSurfaceConductors model) model.thickness
            in
            Svg.g []
                [ viewVisualElement model (ConstructionSegment (cps ++ [ cp ]))
                , viewVisualElement model (ConstructionCrosshair cp)
                ]

        CreateThroughPadTool ->
            viewVisualElement model (ConstructionCircle model.cursor model.radius Nothing)

        CreateNumberedThroughPad pinNumber ->
            viewVisualElement model (ConstructionCircle model.cursor model.radius (Just <| String.fromInt pinNumber))

        CreateDipThroughPad someOfThree ->
            viewDoubleRowTool model someOfThree model.radius ConstructionCircle

        CreateRowThroughPad startNumber someOfTwo ->
            viewRowTool model startNumber someOfTwo model.radius ConstructionCircle

        CreateSurfacePadTool ->
            viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) Nothing)

        CreateNumberedSurfacePad pinNumber ->
            viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just <| String.fromInt pinNumber))

        CreateSoicSurfacePad someOfThree ->
            viewDoubleRowTool model someOfThree (model.radius * 2) ConstructionSquare

        CreateRowSurfacePad startNumber someOfTwo ->
            viewRowTool model startNumber someOfTwo (model.radius * 2) ConstructionSquare

        DefineReferenceFrame mp1 mp2 ->
            viewDefineReferenceFrameTool model mp1 mp2

        CreateDistanceDimension mp1 ->
            viewCreateDistanceDimensionTool model mp1

        CreateAngleDimension mp1 mp2 ->
            viewCreateAngleDimensionTool model mp1 mp2

        _ ->
            Svg.text ""


viewDefineReferenceFrameTool : ModelTools (ModelVisuals (ModelConductors a b)) -> Maybe Point -> Maybe Point -> Svg Visual.Msg
viewDefineReferenceFrameTool model mp1 mp2 =
    let
        cp =
            snapTo model.snapDistance model.cursor model.conductors (activeLayerSurfaceConductors model) model.thickness
    in
    case ( model.ref, mp1, mp2 ) of
        ( Just ref, _, _ ) ->
            Svg.g []
                [ viewVisualElement model (ConstructionCrosshair (FreePoint ref.p1 0))
                , viewVisualElement model (ConstructionCrosshair (FreePoint ref.p2 0))
                ]

        ( Nothing, Nothing, Nothing ) ->
            viewVisualElement model (ConstructionCrosshair cp)

        ( Nothing, Just p1, Nothing ) ->
            Svg.g []
                [ viewVisualElement model (ConstructionCrosshair (FreePoint p1 0))
                , viewVisualElement model (ConstructionCrosshair cp)
                ]

        ( Nothing, Nothing, Just p2 ) ->
            -- TODO create a type that has only these three variants
            Svg.g []
                [ viewVisualElement model (ConstructionCrosshair (FreePoint p2 0))
                , viewVisualElement model (ConstructionCrosshair cp)
                ]

        ( Nothing, Just p1, Just p2 ) ->
            Svg.g []
                [ viewVisualElement model (ConstructionCrosshair (FreePoint p1 0))
                , viewVisualElement model (ConstructionCrosshair (FreePoint p2 0))
                ]


viewCreateDistanceDimensionTool : ModelTools (ModelVisuals (ModelConductors a b)) -> Maybe Point -> Svg Visual.Msg
viewCreateDistanceDimensionTool model mp1 =
    let
        cp =
            snapTo model.snapDistance model.cursor model.conductors (activeLayerSurfaceConductors model) model.thickness
    in
    case mp1 of
        Nothing ->
            viewVisualElement model (ConstructionCrosshair cp)

        Just p1 ->
            Svg.g []
                [ viewVisualElement model (ConstructionCrosshair (FreePoint p1 0))
                , viewVisualElement model (ConstructionLine p1 (constructionPointPoint cp) 1)
                , viewVisualElement model (ConstructionCrosshair cp)
                ]


viewCreateAngleDimensionTool : ModelTools (ModelVisuals (ModelConductors a b)) -> Maybe Point -> Maybe Point -> Svg Visual.Msg
viewCreateAngleDimensionTool model mp1 mp2 =
    let
        cp =
            snapTo model.snapDistance model.cursor model.conductors (activeLayerSurfaceConductors model) model.thickness
    in
    case ( mp1, mp2 ) of
        ( Just p1, Nothing ) ->
            Svg.g []
                [ viewVisualElement model (ConstructionCrosshair (FreePoint p1 0))
                , viewVisualElement model (ConstructionLine p1 (constructionPointPoint cp) 1)
                , viewVisualElement model (ConstructionCrosshair cp)
                ]

        ( Just p1, Just p2 ) ->
            Svg.g []
                [ viewVisualElement model (ConstructionCrosshair (FreePoint p1 0))
                , viewVisualElement model (ConstructionLine p1 p2 1)
                , viewVisualElement model (ConstructionCrosshair (FreePoint p2 0))
                , viewVisualElement model (ConstructionLine p1 (constructionPointPoint cp) 1)
                , viewVisualElement model (ConstructionCrosshair cp)
                ]

        _ ->
            viewVisualElement model (ConstructionCrosshair cp)


viewRowTool : ModelTools (ModelVisuals (ModelConductors a b)) -> Int -> TwoPoints -> Float -> (Point -> Float -> Maybe String -> VisualElement) -> Svg Visual.Msg
viewRowTool model startNumber somePoints size toVisual =
    case somePoints of
        NoneOfTwo ->
            viewVisualElement model (toVisual model.cursor size (Just <| String.fromInt startNumber))

        OneOfTwo p1 ->
            Svg.g []
                [ viewVisualElement model (toVisual p1 size (Just <| String.fromInt startNumber))
                , viewVisualElement model (toVisual model.cursor size (Just <| "?"))
                ]

        TwoOfTwo p1 p2 ->
            let
                shape =
                    generateSingleRow startNumber (model.radius * 2) p1 p2 model.cursor
            in
            Svg.g [] <|
                List.map (\( point, pad ) -> viewVisualElement model (toVisual point size (Maybe.map String.fromInt pad.number))) shape


viewDoubleRowTool : ModelTools (ModelVisuals (ModelConductors a b)) -> ThreePoints -> Float -> (Point -> Float -> Maybe String -> VisualElement) -> Svg Visual.Msg
viewDoubleRowTool model somePoints size toVisual =
    case somePoints of
        -- todo combine code
        NoneOfThree ->
            viewRowTool model 1 NoneOfTwo size toVisual

        OneOfThree p1 ->
            viewRowTool model 1 (OneOfTwo p1) size toVisual

        TwoOfThree p1 p2 ->
            viewRowTool model 1 (TwoOfTwo p1 p2) size toVisual

        ThreeOfThree p1 p2 p3 ->
            let
                shape =
                    generateDoubleRow (model.radius * 2) p1 p2 p3 model.cursor
            in
            Svg.g [] <|
                List.map (\( point, pad ) -> viewVisualElement model (toVisual point size (Maybe.map String.fromInt pad.number))) shape



-- Utility


indexToSurfacePadTool : Int -> Tool
indexToSurfacePadTool index =
    case index of
        2 ->
            resetTool (CreateNumberedSurfacePad 1)

        3 ->
            resetTool (CreateSoicSurfacePad NoneOfThree)

        4 ->
            resetTool (CreateRowSurfacePad 1 NoneOfTwo)

        _ ->
            resetTool CreateSurfacePadTool


indexToThroughPadTool : Int -> Tool
indexToThroughPadTool index =
    case index of
        2 ->
            resetTool (CreateNumberedThroughPad 1)

        3 ->
            resetTool (CreateDipThroughPad NoneOfThree)

        4 ->
            resetTool (CreateRowThroughPad 1 NoneOfTwo)

        _ ->
            resetTool CreateThroughPadTool


indexToDimensionTool : Tool -> Int -> Tool
indexToDimensionTool oldTool index =
    case index of
        1 ->
            resetTool (CreateDistanceDimension Nothing)

        2 ->
            resetTool (CreateAngleDimension Nothing Nothing)

        3 ->
            oldTool

        _ ->
            resetTool (DefineReferenceFrame Nothing Nothing)
