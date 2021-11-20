module Tool exposing (..)

import Common exposing (Point, Radius, Thickness)
import Conductor exposing (Conductor, ConstructionPoint(..), MergeNet(..), ModelConductors, Net(..), SurfaceConductor(..), ThroughConductor(..), activeLayerSurfaceConductors, addSurfaceConductor, addSurfaceConductorNoNet, addThroughConductor, constructionPointsToConductors, constructionPointsToTrace, createTraceToHighlightNets, incrementNextNetId, mergeNets, snapTo, updateConductorNet)
import Svg exposing (Svg)
import Vector exposing (generateDoubleRow, generateSingleRow)
import Visual exposing (ModelVisuals, VisualElement(..), viewVisualElement)



-- MODEL


type Tool
    = SelectTool (Maybe Conductor)
    | CreateTraceTool (List (ConstructionPoint Thickness))
    | CreateSurfacePadTool
    | CreateNumberedSurfacePad Int
    | CreateSoicSurfacePad (Maybe Point) (Maybe Point)
    | CreateRowSurfacePad Int (Maybe Point) (Maybe Point) -- TODO row should support setting starting pin number
    | CreateThroughPadTool
    | CreateNumberedThroughPad Int
    | CreateDipThroughPad (Maybe Point) (Maybe Point)
    | CreateRowThroughPad (Maybe Point) (Maybe Point)
    | CreateZoneTool


resetTool : Tool -> Tool
resetTool tool =
    case tool of
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

        CreateNumberedSurfacePad _ ->
            CreateNumberedSurfacePad 1

        CreateSoicSurfacePad _ _ ->
            CreateSoicSurfacePad Nothing Nothing

        CreateRowSurfacePad _ _ _ ->
            CreateRowSurfacePad 1 Nothing Nothing

        CreateNumberedThroughPad _ ->
            CreateNumberedThroughPad 1

        CreateDipThroughPad _ _ ->
            CreateDipThroughPad Nothing Nothing

        CreateRowThroughPad _ _ ->
            CreateRowThroughPad Nothing Nothing


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

        CreateNumberedSurfacePad _ ->
            "numbered-surface"

        CreateSoicSurfacePad _ _ ->
            "soic-surface"

        CreateRowSurfacePad _ _ _ ->
            "row-surface"

        CreateNumberedThroughPad _ ->
            "numbered-thorugh"

        CreateDipThroughPad _ _ ->
            "dip-through"

        CreateRowThroughPad _ _ ->
            "row-through"



-- UPDATE


type Msg
    = LeftClick Point
    | ShiftScroll Float
    | CtrlScroll Float
    | SetTool Tool
    | Reset
    | SetSubTool Int


type alias ModelTools a =
    { a
        | tool : Tool
        , thickness : Thickness
        , radius : Radius
        , cursor : Point
        , snapDistance : Float
    }


update : Msg -> ModelTools (ModelVisuals (ModelConductors a b)) -> ( ModelTools (ModelVisuals (ModelConductors a b)), Cmd msg, Bool )
update msg model =
    case msg of
        LeftClick point ->
            let
                snapPoint =
                    snapTo model.snapDistance point model.conductors (activeLayerSurfaceConductors model)
            in
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
                                ( { model | tool = CreateTraceTool t2 }, Cmd.none, False )

                CreateThroughPadTool ->
                    ( addThroughConductor (ThroughPad { number = Nothing, label = Nothing } point model.radius) model, Cmd.none, True )

                CreateSurfacePadTool ->
                    ( addSurfaceConductorNoNet (SurfacePad { number = Nothing, label = Nothing } point (model.radius * 2)) model, Cmd.none, True )

                CreateNumberedSurfacePad pinNumber ->
                    ( { model | tool = CreateNumberedSurfacePad (pinNumber + 1) }
                        |> addSurfaceConductorNoNet (SurfacePad { number = Just pinNumber, label = Nothing } point (model.radius * 2))
                    , Cmd.none
                    , True
                    )

                CreateSoicSurfacePad mp1 mp2 ->
                    case ( mp1, mp2 ) of
                        ( Just _, Nothing ) ->
                            ( { model | tool = CreateSoicSurfacePad mp1 (Just point) }, Cmd.none, False )

                        ( Just p1, Just p2 ) ->
                            let
                                shape =
                                    generateDoubleRow p1 p2 model.cursor
                            in
                            ( List.foldl
                                (\( shapePoint, shapePad ) m ->
                                    addSurfaceConductorNoNet (SurfacePad shapePad shapePoint (model.radius * 2)) m
                                )
                                { model | tool = CreateSoicSurfacePad Nothing Nothing }
                                shape
                            , Cmd.none
                            , True
                            )

                        _ ->
                            ( { model | tool = CreateSoicSurfacePad (Just point) Nothing }, Cmd.none, False )

                CreateRowSurfacePad startNumber mp1 mp2 ->
                    case ( mp1, mp2 ) of
                        ( Just _, Nothing ) ->
                            ( { model | tool = CreateRowSurfacePad startNumber mp1 (Just point) }, Cmd.none, False )

                        ( Just p1, Just p2 ) ->
                            let
                                shape =
                                    generateSingleRow startNumber p1 p2 model.cursor

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
                                { model | tool = CreateRowSurfacePad lastPinNumber Nothing Nothing }
                                shape
                            , Cmd.none
                            , True
                            )

                        _ ->
                            ( { model | tool = CreateRowSurfacePad startNumber (Just point) Nothing }, Cmd.none, False )

                _ ->
                    ( model, Cmd.none, False )

        ShiftScroll delta ->
            case model.tool of
                CreateThroughPadTool ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateSurfacePadTool ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateNumberedSurfacePad _ ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateSoicSurfacePad _ _ ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateRowSurfacePad _ _ _ ->
                    ( updateRadius delta model, Cmd.none, False )

                CreateTraceTool _ ->
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
                CreateRowSurfacePad startNumber p1 p2 ->
                    ( { model | tool = CreateRowSurfacePad (max 1 (startNumber + step)) p1 p2 }, Cmd.none, False )

                _ ->
                    ( model, Cmd.none, False )

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
                    ( { model | tool = tool, highlightNets = [] }, Cmd.none, False )

        Reset ->
            ( resetModelTool model, Cmd.none, False )

        SetSubTool index ->
            case model.tool of
                SelectTool _ ->
                    ( model, Cmd.none, False )

                CreateTraceTool _ ->
                    ( model, Cmd.none, False )

                CreateSurfacePadTool ->
                    ( { model | tool = indexToSurfacePadTool index }, Cmd.none, False )

                CreateNumberedSurfacePad _ ->
                    ( { model | tool = indexToSurfacePadTool index }, Cmd.none, False )

                CreateSoicSurfacePad _ _ ->
                    ( { model | tool = indexToSurfacePadTool index }, Cmd.none, False )

                CreateRowSurfacePad _ _ _ ->
                    ( { model | tool = indexToSurfacePadTool index }, Cmd.none, False )

                CreateThroughPadTool ->
                    ( model, Cmd.none, False )

                CreateNumberedThroughPad _ ->
                    ( model, Cmd.none, False )

                CreateDipThroughPad _ _ ->
                    ( model, Cmd.none, False )

                CreateRowThroughPad _ _ ->
                    ( model, Cmd.none, False )

                CreateZoneTool ->
                    ( model, Cmd.none, False )


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
        CreateTraceTool cps ->
            let
                cp =
                    snapTo model.snapDistance model.cursor model.conductors (activeLayerSurfaceConductors model) model.thickness
            in
            Svg.g []
                [ viewVisualElement model (ConstructionSegment (cps ++ [ cp ]))
                , viewVisualElement model (ConstructionCrosshair cp)
                ]

        CreateThroughPadTool ->
            viewVisualElement model (ConstructionCircle model.cursor model.radius)

        CreateSurfacePadTool ->
            viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) Nothing)

        CreateNumberedSurfacePad pinNumber ->
            viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just <| String.fromInt pinNumber))

        CreateSoicSurfacePad mp1 mp2 ->
            case ( mp1, mp2 ) of
                -- todo simplify
                ( Just p1, Nothing ) ->
                    Svg.g []
                        [ viewVisualElement model (ConstructionSquare p1 (model.radius * 2) (Just "1"))
                        , viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just "2"))
                        ]

                ( Just p1, Just p2 ) ->
                    let
                        shape =
                            generateDoubleRow p1 p2 model.cursor
                    in
                    Svg.g [] <|
                        List.map (\( point, pad ) -> viewVisualElement model (ConstructionSquare point (model.radius * 2) (Maybe.map String.fromInt pad.number))) shape

                _ ->
                    viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just "1"))

        CreateRowSurfacePad startNumber mp1 mp2 ->
            case ( mp1, mp2 ) of
                ( Just p1, Nothing ) ->
                    Svg.g []
                        [ viewVisualElement model (ConstructionSquare p1 (model.radius * 2) (Just <| String.fromInt startNumber))
                        , viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just <| String.fromInt (startNumber + 1)))
                        ]

                ( Just p1, Just p2 ) ->
                    let
                        shape =
                            generateSingleRow startNumber p1 p2 model.cursor
                    in
                    Svg.g [] <|
                        List.map (\( point, pad ) -> viewVisualElement model (ConstructionSquare point (model.radius * 2) (Maybe.map String.fromInt pad.number))) shape

                _ ->
                    viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just <| String.fromInt startNumber))

        _ ->
            Svg.text ""



-- Utility


indexToSurfacePadTool : Int -> Tool
indexToSurfacePadTool index =
    case index of
        2 ->
            resetTool (CreateNumberedSurfacePad 1)

        3 ->
            resetTool (CreateSoicSurfacePad Nothing Nothing)

        4 ->
            resetTool (CreateRowSurfacePad 1 Nothing Nothing)

        _ ->
            resetTool CreateSurfacePadTool
