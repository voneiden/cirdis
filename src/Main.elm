port module Main exposing (..)

import Base64
import Browser exposing (Document)
import Browser.Events exposing (onKeyDown, onKeyUp, onMouseUp)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Svg.Lazy as Svg
import Task



-- Todo things
-- allow scaling line segment width
-- copy segment width to next segment (set it actually to model so it stays on?)
-- placing pins and components?
-- staging area
-- via?
-- copper pours
-- undo/redo
-- snapping can be optimized with some kind of dict mapping to pixel regions


port mouseDrag : (MousePosition -> msg) -> Sub msg


port startDrag : () -> Cmd msg


port wheel : (Float -> msg) -> Sub msg


port startWheel : () -> Cmd msg


port endWheel : () -> Cmd msg


port resize : (BoundingClientRect -> msg) -> Sub msg


port canvasSize : () -> Cmd msg


port checkImages : () -> Cmd msg


port imageInformation : (List ImageInformation -> msg) -> Sub msg


type alias BoundingClientRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , top : Float
    }


type alias ImageInformation =
    { layer : String
    , height : Float
    , width : Float
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
-- The app needs to scale the svg with dom changes, but for now lets hardcode


type LayerError
    = RasterB64EncodeFailed


type alias LayerData =
    RemoteData LayerError LayerDefinition


type alias Model =
    { loadLayerStatus : LayerData
    , layers : List LayerDefinition
    , transform : Transform
    , focused : Bool
    , dragging : Bool
    , shift : Bool
    , ctrl : Bool
    , lastMousePosition : MousePosition
    , canvasBoundingClientRect : BoundingClientRect
    , trace : List Point
    , radius : Float
    , thickness : Float
    , tool : Tool
    , conductors : List ThroughConductor
    , nextNetId : Int -- Running id for nets
    , netInfo : Dict String NetInfo
    , snapDistance : Float
    , autoNetColor : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model RemoteData.NotAsked
        []
        defaultTransform
        False
        False
        False
        False
        defaultMousePosition
        defaultBoundingClientRect
        []
        5
        10
        (SelectConductor Nothing)
        []
        1
        Dict.empty
        20
        "#aaaaaa"
    , canvasSize ()
    )


type alias Point =
    { x : Float
    , y : Float
    }



-- traces, zones, nets, pads, components
-- components contain pads
-- nets contain traces, zones and pads


type Tool
    = SelectConductor (Maybe Conductor)
    | SelectNet (Maybe Net)
    | CreateTrace (List (ConstructionPoint Thickness))
    | CreateSurfacePad
    | CreateThroughPad
    | CreateZone


type alias NetInfo =
    { color : String
    }


type Net
    = AutoNet Int
    | CustomNet String String



--type alias Component =
--    { name : String
--    , pads : List Conductor
--    }


type Conductor
    = Surface SurfaceConductor
    | Through ThroughConductor


type SurfaceConductor
    = Trace (List TracePoint) Net
    | SurfacePad Point Width Net
    | Zone (List Point) Net


type ThroughConductor
    = ThroughPad Point Radius Net


type alias NetId =
    Int


type alias Radius =
    Float


type alias Thickness =
    Float


type alias Width =
    Float


type alias TracePoint =
    { point : Point
    , thickness : Float
    }



--type alias TraceData a =
--    { a | thickness : Float}


pointToTracePoint : Point -> Float -> TracePoint
pointToTracePoint point thickness =
    { point = point, thickness = thickness }


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
translateTransform : BoundingClientRect -> Transform -> Float -> Float -> Transform
translateTransform r t x y =
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


{-| Convert a Transform into a SVG viewBox attribute value
-}
transformToViewBox : BoundingClientRect -> Transform -> String
transformToViewBox r t =
    let
        width =
            r.width * t.z

        height =
            r.height * t.z
    in
    String.join " " [ String.fromFloat <| t.x - (width / 2), String.fromFloat <| t.y - (height / 2), String.fromFloat <| width, String.fromFloat <| height ]


defaultMousePosition : MousePosition
defaultMousePosition =
    { timeStamp = 0, offsetX = 0, offsetY = 0, button = 0 }


defaultBoundingClientRect : BoundingClientRect
defaultBoundingClientRect =
    { x = 0, y = 0, width = 500, height = 500, top = 0 }



-- UPDATE


type alias LayerDefinition =
    { title : String
    , mimeType : String
    , b64Data : String
    , opacity : Int
    , conductors : List SurfaceConductor
    }


type alias FileInfo =
    { name : String
    , mime : String
    , data : Bytes
    }


type Msg
    = GetLayerImage
    | GotLayerImage File
    | GotLayerDefinition FileInfo
    | RemoveLayer Int
    | MouseDown MousePosition
    | MouseMove MousePosition
    | MouseUp MousePosition
    | MouseOver
    | MouseOut
    | MouseWheel Float
    | Resize BoundingClientRect
    | GotImageInformation (List ImageInformation)
    | KeyDown Key
    | KeyUp Key
    | SetTool Tool


{-| Take the first element of a list and move it as the last element

If you wanna go the other way, use List.reverse I guess

-}
cycle : List a -> List a
cycle list =
    case list of
        a :: b ->
            b ++ [ a ]

        [] ->
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetLayerImage ->
            ( model
            , Select.file [ "image/jpeg", "image/png" ] GotLayerImage
            )

        GotLayerImage file ->
            ( model
            , Task.perform GotLayerDefinition (File.toBytes file |> Task.map (\bytes -> { name = File.name file, mime = File.mime file, data = bytes }))
            )

        GotLayerDefinition fileInfo ->
            case Base64.fromBytes fileInfo.data of
                Just b64 ->
                    let
                        layerDefinition =
                            { title = fileInfo.name
                            , mimeType = fileInfo.mime
                            , b64Data = b64
                            , opacity = 100
                            , conductors = []
                            }
                    in
                    ( { model | layers = layerDefinition :: model.layers }
                    , Cmd.batch [ canvasSize (), checkImages () ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemoveLayer index ->
            ( { model | layers = List.take index model.layers ++ List.drop (index + 1) model.layers }, Cmd.none )

        MouseDown mousePosition ->
            -- Middle mouse button starts drag
            case mousePosition.button of
                1 ->
                    ( { model | lastMousePosition = mousePosition, dragging = True }, Cmd.none )

                0 ->
                    updateTool msg model

                _ ->
                    ( model, Cmd.none )

        MouseUp mousePosition ->
            case mousePosition.button of
                1 ->
                    ( { model | dragging = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseMove mousePosition ->
            if model.dragging then
                let
                    dx =
                        model.lastMousePosition.offsetX - mousePosition.offsetX

                    dy =
                        model.lastMousePosition.offsetY - mousePosition.offsetY
                in
                ( { model | lastMousePosition = mousePosition, transform = translateTransform model.canvasBoundingClientRect model.transform dx dy }, Cmd.none )

            else
                ( { model | lastMousePosition = mousePosition }, Cmd.none )

        MouseOver ->
            ( { model | focused = True }, startWheel () )

        MouseOut ->
            ( { model | focused = False }, endWheel () )

        MouseWheel delta ->
            case ( model.shift, model.ctrl ) of
                ( False, False ) ->
                    ( { model | transform = zoomTransform model.transform delta }, Cmd.none )

                ( True, False ) ->
                    updateTool msg model

                _ ->
                    ( model, Cmd.none )

        Resize boundingClientRect ->
            -- TODO store this somewhere
            ( { model | canvasBoundingClientRect = boundingClientRect }, Cmd.none )

        GotImageInformation imageInformations ->
            case List.take 1 imageInformations of
                info :: _ ->
                    let
                        -- Frame the layer nicely
                        widthRatio =
                            info.width / model.canvasBoundingClientRect.width

                        heightRatio =
                            info.height / model.canvasBoundingClientRect.height
                    in
                    ( { model
                        | transform =
                            { x = info.width / 2
                            , y = info.height / 2
                            , z = max widthRatio heightRatio + 0.1
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        KeyDown key ->
            case key.keyCode of
                86 ->
                    ( { model | layers = cycle model.layers }, Cmd.none )

                16 ->
                    ( { model | shift = True }, Cmd.none )

                17 ->
                    ( { model | ctrl = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            case key.keyCode of
                16 ->
                    ( { model | shift = False }, Cmd.none )

                17 ->
                    ( { model | ctrl = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetTool tool ->
            ( { model | tool = tool }, Cmd.none )


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


constructionPointToTracePoint : ConstructionPoint Thickness -> TracePoint
constructionPointToTracePoint cp =
    { point = constructionPointPoint cp, thickness = constructionPointA cp }


constructionPointsToTracePoints : List (ConstructionPoint Thickness) -> List TracePoint
constructionPointsToTracePoints cps =
    List.map constructionPointToTracePoint cps


constructionPointsToTrace : List (ConstructionPoint Thickness) -> SurfaceConductor
constructionPointsToTrace points =
    -- todo net
    Trace (constructionPointsToTracePoints points) (AutoNet 0)


{-| Come up with some neat way to test if the conductor should connect to some existing net
-}



-- TODO some kind of netsolver that can also check for broken nets and split the data appropriately
-- TODO kinda wanna check for collisions too!


addThroughConductor : (Net -> ThroughConductor) -> Model -> Model
addThroughConductor toConductor model =
    case toConductor (AutoNet 0) of
        ThroughPad point radius _ ->
            { model | nextNetId = model.nextNetId + 1, conductors = toConductor (AutoNet model.nextNetId) :: model.conductors }


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


updateTool : Msg -> Model -> ( Model, Cmd Msg )
updateTool msg model =
    case msg of
        MouseDown mousePosition ->
            let
                mousePoint =
                    mousePositionToPoint model.canvasBoundingClientRect model.transform mousePosition

                snapPoint =
                    snapTo model.snapDistance mousePoint model.conductors (activeLayerSurfaceConductors model)
            in
            case model.tool of
                CreateTrace points ->
                    case snapPoint model.thickness of
                        SnapPoint p c t ->
                            -- TODO finalize trace here
                            let
                                newPoints =
                                    points ++ [ SnapPoint p c t ]
                            in
                            if List.isEmpty points then
                                ( { model | tool = CreateTrace newPoints }, Cmd.none )

                            else
                                -- TODO how do we merge nets?
                                let
                                    newModel =
                                        addSurfaceConductor (constructionPointsToTrace newPoints) model
                                in
                                ( { newModel | tool = CreateTrace [] }, Cmd.none )

                        FreePoint p t ->
                            let
                                t2 =
                                    points ++ [ FreePoint p t ]
                            in
                            ( { model | tool = CreateTrace t2 }, Cmd.none )

                CreateThroughPad ->
                    ( addThroughConductor (ThroughPad mousePoint model.radius) model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseWheel delta ->
            case model.tool of
                CreateThroughPad ->
                    ( updateRadius delta model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


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


{-| Distance between two points
-}
distanceToPoint : Point -> Point -> Float
distanceToPoint p1 p2 =
    sqrt ((p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2)


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


throughConductorPoints : ThroughConductor -> List Point
throughConductorPoints tht =
    case tht of
        ThroughPad point _ _ ->
            [ point ]


surfaceConductorPoints : SurfaceConductor -> List Point
surfaceConductorPoints smt =
    case smt of
        Trace tracePoints _ ->
            List.map .point tracePoints

        SurfacePad point _ _ ->
            [ point ]

        Zone points _ ->
            points


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



--let
--    distances =
--        List.sortBy (\( distance, _ ) -> distance) <|
--            List.map (\c -> ( distanceToConductor point c, c )) <|
--
--in
--Nothing


activeLayerSurfaceConductors : Model -> List SurfaceConductor
activeLayerSurfaceConductors model =
    Maybe.withDefault [] (Maybe.map (\l -> l.conductors) (List.head model.layers))


conductorNet : Conductor -> Net
conductorNet conductor =
    case conductor of
        Through tht ->
            throughConductorNet tht

        Surface smt ->
            surfaceConductorNet smt


throughConductorNet : ThroughConductor -> Net
throughConductorNet tht =
    case tht of
        ThroughPad _ _ net ->
            net


surfaceConductorNet : SurfaceConductor -> Net
surfaceConductorNet smt =
    case smt of
        Trace _ net ->
            net

        SurfacePad _ _ net ->
            net

        Zone _ net ->
            net



--pointNet : Model -> Point -> List ThroughConductor -> List SurfaceConductor -> Net
--pointNet model point thts smts =
--    case snapTo model.snapDistance point thts smts of
--        SnapPoint _ c _ ->
--            conductorNet c
--
--        FreePoint _ _ ->
--            AutoNet 0 -- TODO?
-- VIEW


view : Model -> Document Msg
view model =
    { title = "Circuit Dissector"
    , body =
        [ lazy
            (\m ->
                div
                    [ id "root"
                    ]
                    [ div [ class "flex-row" ]
                        [ div
                            [ id "canvas-container"
                            , onMouseEnter MouseOver
                            , onMouseLeave MouseOut
                            ]
                            [ Svg.svg
                                [ SvgA.id "canvas"
                                , SvgE.preventDefaultOn "mousedown" (Decode.map (\msg -> ( msg, True )) (Decode.map MouseDown decodeMousePosition))
                                , SvgA.viewBox <| transformToViewBox model.canvasBoundingClientRect model.transform
                                , SvgA.preserveAspectRatio "slice"
                                ]
                                ([ Maybe.withDefault (text "") <| Maybe.map viewLayer <| List.head model.layers
                                 ]
                                    ++ [ viewTool model
                                       ]
                                    ++ viewMaybeLayerSurfaceConductors (List.head model.layers)
                                    ++ List.map viewThroughConductor model.conductors
                                )
                            ]
                        , div [ id "right-menu" ]
                            [ viewLayerList model.layers
                            , viewLayerSelect model.loadLayerStatus
                            , div [] [ button [ onClick <| SetTool <| SelectConductor Nothing ] [ text "Select Conductor" ] ]
                            , div [] [ button [ onClick <| SetTool <| SelectNet Nothing ] [ text "Select Net" ] ]
                            , div [] [ button [ onClick <| SetTool <| CreateThroughPad ] [ text "Create THT Pad/Via" ] ]
                            , div [] [ button [ onClick <| SetTool <| CreateSurfacePad ] [ text "Create SMT Pad" ] ]
                            , div [] [ button [ onClick <| SetTool <| CreateTrace [] ] [ text "Create Trace" ] ]
                            ]
                        ]
                    ]
            )
            model
        ]
    }


viewLayer : LayerDefinition -> Svg Msg
viewLayer layer =
    if layer.opacity == 0 then
        Svg.text ""

    else
        Svg.lazy
            (\l ->
                Svg.image
                    [ SvgA.class <| "layer-" ++ l.title
                    , SvgA.xlinkHref <| "data:" ++ l.mimeType ++ ";base64," ++ l.b64Data
                    ]
                    []
            )
            layer


fromPoint : String -> Point -> String
fromPoint cmd point =
    cmd ++ " " ++ String.fromFloat point.x ++ "," ++ String.fromFloat point.y


{-| Take elements out of list while elements match a certain criteria
and return a tuple of matched and unmatched elements
-}
takeWhile : (a -> Bool) -> List a -> ( List a, List a )
takeWhile test list =
    case list of
        a :: rest ->
            if test a then
                let
                    ( matched, unmatched ) =
                        takeWhile test rest
                in
                ( a :: matched, unmatched )

            else
                ( [], a :: rest )

        _ ->
            ( [], [] )



--
-- TODO use this, but I guess shorten the var names a bit first


type alias CoordinateConversion a =
    { a | canvasBoundingClientRect : BoundingClientRect, transform : Transform }


mousePositionToPoint : BoundingClientRect -> Transform -> MousePosition -> Point
mousePositionToPoint b t mousePosition =
    let
        deltaX =
            mousePosition.offsetX - b.width / 2

        deltaY =
            mousePosition.offsetY - b.height / 2
    in
    { x = t.x + deltaX * t.z, y = t.y + deltaY * t.z }


{-| Display anything and everything related to the active tool
-}
viewTool : Model -> Svg Msg
viewTool model =
    case model.tool of
        CreateTrace cps ->
            viewTraceWithConstruction model (constructionPointsToTracePoints cps)

        CreateThroughPad ->
            let
                point =
                    mousePositionToPoint model.canvasBoundingClientRect model.transform model.lastMousePosition
            in
            viewThroughConductor <| ThroughPad point model.radius (AutoNet 0)

        _ ->
            Svg.text ""


viewTrace : List TracePoint -> Svg Msg
viewTrace points =
    Svg.g [] (viewTraceSegmented points)


viewTraceWithConstruction : Model -> List TracePoint -> Svg Msg
viewTraceWithConstruction model points =
    let
        constructionTrace =
            if model.focused then
                [ viewConstructionTrace model points ]

            else
                []
    in
    Svg.g [] <|
        viewTraceSegmented points
            ++ constructionTrace


pathFromPoints : List TracePoint -> Svg Msg
pathFromPoints tracePoints =
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
                , SvgA.stroke "red"
                , SvgA.strokeWidth (String.fromFloat start.thickness)
                , SvgA.strokeLinecap "round"
                ]
                []

        _ ->
            Svg.text ""


viewTraceSegmented : List TracePoint -> List (Svg Msg)
viewTraceSegmented tracePoints =
    -- TODO add some intelligence here and merge lines that have the same thickness
    -- use takeWhile here
    case tracePoints of
        start :: end :: rest ->
            pathFromPoints [ start, end ] :: viewTraceSegmented (end :: rest)

        _ ->
            [ Svg.text "" ]


viewConstructionTrace : Model -> List TracePoint -> Svg Msg
viewConstructionTrace model tracePoints =
    case List.reverse tracePoints of
        last :: _ ->
            let
                mousePoint =
                    mousePositionToPoint model.canvasBoundingClientRect model.transform model.lastMousePosition

                point =
                    case snapTo model.snapDistance mousePoint model.conductors (activeLayerSurfaceConductors model) model.thickness of
                        SnapPoint p _ _ ->
                            p

                        FreePoint p _ ->
                            p
            in
            pathFromPoints [ last, pointToTracePoint point model.thickness ]

        _ ->
            Svg.text ""


viewMaybeLayerSurfaceConductors : Maybe LayerDefinition -> List (Svg Msg)
viewMaybeLayerSurfaceConductors maybeLayer =
    case maybeLayer of
        Just layer ->
            List.map viewSurfaceConductor layer.conductors

        Nothing ->
            []


viewSurfaceConductor : SurfaceConductor -> Svg Msg
viewSurfaceConductor surfaceConductor =
    case surfaceConductor of
        Trace tracePoints net ->
            viewTrace tracePoints

        SurfacePad point width net ->
            Svg.text "SurfacePad"

        Zone points net ->
            Svg.text "ZONE"


viewThroughConductor : ThroughConductor -> Svg Msg
viewThroughConductor throughConductor =
    case throughConductor of
        ThroughPad point radius netId ->
            Svg.circle
                [ SvgA.cx <| String.fromFloat point.x
                , SvgA.cy <| String.fromFloat point.y
                , SvgA.r <| String.fromFloat radius
                ]
                []


viewLayerList : List LayerDefinition -> Html Msg
viewLayerList layers =
    div [ class "layer-list" ] <|
        List.indexedMap viewLayerControls layers


viewLayerControls : Int -> LayerDefinition -> Html Msg
viewLayerControls index layer =
    div [ class "layer-info" ]
        [ span [] [ text layer.title ]
        , span [ onClick <| RemoveLayer index ] [ text "[Remove]" ]
        ]


viewLayerSelect : LayerData -> Html Msg
viewLayerSelect layerData =
    case layerData of
        RemoteData.NotAsked ->
            div []
                [ --input [ placeholder "New Layer", value title ] []
                  button [ onClick <| GetLayerImage ] [ text <| "Import layer" ]
                ]

        RemoteData.Loading ->
            div []
                [ text "Loading.." ]

        RemoteData.Failure error ->
            div []
                [ text "Error todo" ]

        -- Todo shouldn't get this one tbh
        RemoteData.Success result ->
            div []
                [ text "Success todo" ]


type alias MousePosition =
    { timeStamp : Float
    , offsetX : Float
    , offsetY : Float
    , button : Int
    }


decodeMousePosition : Decoder MousePosition
decodeMousePosition =
    Decode.map4 MousePosition
        (Decode.field "timeStamp" Decode.float)
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        (Decode.field "button" Decode.int)


type alias Key =
    { keyCode : Int
    , shift : Bool
    , ctrl : Bool
    }


decodeKey : Decoder Key
decodeKey =
    Decode.map3 Key
        (Decode.field "keyCode" Decode.int)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ mouseDrag MouseMove
        , wheel MouseWheel
        , resize Resize
        , imageInformation GotImageInformation
        , onKeyDown (decodeKey |> Decode.map KeyDown)
        , onKeyUp (decodeKey |> Decode.map KeyUp)
        , onMouseUp (Decode.map MouseUp decodeMousePosition)
        ]
