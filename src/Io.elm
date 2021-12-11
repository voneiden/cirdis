module Io exposing (..)

-- Main model

import Common exposing (Dimension(..), Pad, PadLabel, Point, ReferenceFrame)
import Conductor exposing (ConstructionPoint(..), Net(..), SurfaceConductor(..), ThroughConductor(..), TracePoint)
import Dict exposing (Dict)
import Form
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import VirtualDom
import Visual
import Workspace


type alias MainModel =
    { layers : Dict Int LayerData
    , dragging : Bool
    , shift : Bool
    , ctrl : Bool
    , lastMousePosition : MousePosition
    , canvasBoundingClientRect : BoundingClientRect
    , timeline : WorkspaceTimeline
    , keyDownPreventDefault : Bool
    , ePressed : Bool
    , zPressed : Bool
    , xPressed : Bool
    , vPressed : Bool
    , includeSource : Bool
    }


type alias LayerData =
    { id : Int
    , title : String
    , mimeType : String
    , b64Data : String
    }


type alias MousePosition =
    { timeStamp : Float
    , offsetX : Float
    , offsetY : Float
    , button : Int
    }


type alias BoundingClientRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , top : Float
    }


type alias WorkspaceTimeline =
    { current : Workspace.Model
    , past : List Workspace.Model
    , future : List Workspace.Model
    }


defaultWorkspaceTimeline : WorkspaceTimeline
defaultWorkspaceTimeline =
    { current = Workspace.defaultModel
    , past = []
    , future = []
    }


encodedSvgModel : MainModel -> Svg msg
encodedSvgModel model =
    Svg.g
        [ SvgA.id "cirdis-src"
        , VirtualDom.attribute "data-cirdis-src" (Encode.encode 0 (encodeMainModel model))
        ]
        []


encodeMainModel : MainModel -> Encode.Value
encodeMainModel model =
    Encode.object
        [ ( "version", Encode.string "1.0-alpha" )
        , ( "layers", Encode.list encodeLayerData (Dict.toList model.layers) )
        , ( "workspace", encodeWorkspace model.timeline.current )
        ]


decodeMainModel : MainModel -> List ( Int, String ) -> Decoder MainModel
decodeMainModel last layerB64DataList =
    let
        layerB64DataMap =
            Dict.fromList layerB64DataList
    in
    Decode.succeed MainModel
        |> required "layers"
            (Decode.map Dict.fromList
                (Decode.list
                    (Decode.map
                        (\ld -> ( ld.id, ld ))
                        (decodeLayerData layerB64DataMap)
                    )
                )
            )
        |> hardcoded last.dragging
        |> hardcoded last.shift
        |> hardcoded last.ctrl
        |> hardcoded last.lastMousePosition
        |> hardcoded last.canvasBoundingClientRect
        |> required "workspace" (Decode.map (\ws -> { defaultWorkspaceTimeline | current = ws }) (decodeWorkspace last.timeline.current))
        |> hardcoded last.keyDownPreventDefault
        |> hardcoded last.ePressed
        |> hardcoded last.zPressed
        |> hardcoded last.xPressed
        |> hardcoded last.vPressed
        |> hardcoded last.includeSource


decodeLayerData : Dict Int String -> Decoder LayerData
decodeLayerData layerB64DataMap =
    Decode.map4
        LayerData
        (Decode.field "id" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "mimeType" Decode.string)
        (Decode.field "id"
            (Decode.int
                |> Decode.andThen
                    (\id ->
                        case Dict.get id layerB64DataMap of
                            Just b64Data ->
                                Decode.succeed b64Data

                            Nothing ->
                                Decode.fail "Layer data missing"
                    )
            )
        )


encodeLayerData : ( Int, LayerData ) -> Encode.Value
encodeLayerData ( id, layerData ) =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "title", Encode.string layerData.title )
        , ( "mimeType", Encode.string layerData.mimeType )
        ]


encodeWorkspace : Workspace.Model -> Encode.Value
encodeWorkspace ws =
    Encode.object
        [ ( "layers", Encode.list encodeLayer ws.layers )
        , ( "transform", encodeTransform ws.transform )
        , ( "radius", Encode.float ws.radius )
        , ( "thickness", Encode.float ws.thickness )
        , ( "conductors", Encode.list encodeThroughConductor ws.conductors )
        , ( "nextNetId", Encode.int ws.nextNetId )
        , ( "snapDistance", Encode.float ws.snapDistance )
        , ( "ref", encodeMaybeReferenceFrame ws.ref )
        , ( "dimensions", Encode.list encodeDimension ws.dimensions )
        ]


decodeWorkspace : Workspace.Model -> Decoder Workspace.Model
decodeWorkspace last =
    Decode.succeed Workspace.Model
        |> required "layers" (Decode.list decodeLayer)
        |> hardcoded last.cursor
        |> hardcoded (FreePoint last.cursor ())
        |> hardcoded last.focused
        |> required "transform" decodeTransform
        |> hardcoded last.canvas
        |> required "radius" Decode.float
        |> required "thickness" Decode.float
        |> hardcoded last.tool
        |> required "conductors" (Decode.list decodeThroughConductor)
        |> required "nextNetId" Decode.int
        |> required "snapDistance" Decode.float
        |> hardcoded last.autoNetColor
        |> required "ref" (Decode.nullable decodeReferenceFrame)
        |> hardcoded Form.NoForm
        |> required "dimensions" (Decode.list decodeDimension)
        |> hardcoded Visual.Background


encodeLayer : Workspace.Layer -> Encode.Value
encodeLayer layer =
    Encode.object
        [ ( "id", Encode.int layer.id )
        , ( "opacity", Encode.int layer.opacity )
        , ( "conductors", Encode.list encodeSurfaceConductor layer.conductors )
        ]


decodeLayer : Decoder Workspace.Layer
decodeLayer =
    Decode.map3
        Workspace.Layer
        (Decode.field "id" Decode.int)
        (Decode.field "opacity" Decode.int)
        (Decode.field "conductors" (Decode.list decodeSurfaceConductor))


encodeTransform : Workspace.Transform -> Encode.Value
encodeTransform transform =
    Encode.object
        [ ( "x", Encode.float transform.x )
        , ( "y", Encode.float transform.y )
        , ( "z", Encode.float transform.z )
        ]


decodeTransform : Decoder Workspace.Transform
decodeTransform =
    Decode.map3
        Workspace.Transform
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "z" Decode.float)


encodeMaybeReferenceFrame : Maybe ReferenceFrame -> Encode.Value
encodeMaybeReferenceFrame mrf =
    case mrf of
        Nothing ->
            Encode.null

        Just rf ->
            Encode.object
                [ ( "p1", encodePoint rf.p1 )
                , ( "p2", encodePoint rf.p2 )
                , ( "value", Encode.float rf.value )
                , ( "unit", Encode.string rf.unit )
                , ( "ratio", Encode.float rf.ratio )
                ]


decodeReferenceFrame : Decoder ReferenceFrame
decodeReferenceFrame =
    Decode.map5
        ReferenceFrame
        (Decode.field "p1" decodePoint)
        (Decode.field "p2" decodePoint)
        (Decode.field "value" Decode.float)
        (Decode.field "unit" Decode.string)
        (Decode.field "ratio" Decode.float)


encodePoint : Point -> Encode.Value
encodePoint point =
    Encode.object
        [ ( "x", Encode.float point.x )
        , ( "y", Encode.float point.y )
        ]


decodePoint : Decoder Point
decodePoint =
    Decode.map2
        Point
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


encodeThroughConductor : ThroughConductor -> Encode.Value
encodeThroughConductor tc =
    case tc of
        ThroughPad pad point radius net ->
            Encode.object
                [ ( "type", Encode.string "through-pad" )
                , ( "pad", encodePad pad )
                , ( "point", encodePoint point )
                , ( "radius", Encode.float radius )
                , ( "net", encodeNet net )
                ]


decodeThroughConductor : Decoder ThroughConductor
decodeThroughConductor =
    Decode.map4
        ThroughPad
        (Decode.field "pad" decodePad)
        (Decode.field "point" decodePoint)
        (Decode.field "radius" Decode.float)
        (Decode.field "net" decodeNet)


encodeSurfaceConductor : SurfaceConductor -> Encode.Value
encodeSurfaceConductor sc =
    case sc of
        Trace tracePoints net ->
            Encode.object
                [ ( "type", Encode.string "trace" )
                , ( "tracePoints", Encode.list encodeTracePoint tracePoints )
                , ( "net", encodeNet net )
                ]

        SurfacePad pad point width net ->
            Encode.object
                [ ( "type", Encode.string "surface-pad" )
                , ( "pad", encodePad pad )
                , ( "point", encodePoint point )
                , ( "width", Encode.float width )
                , ( "net", encodeNet net )
                ]

        Zone points net ->
            Encode.object
                [ ( "type", Encode.string "zone" )
                , ( "points", Encode.list encodePoint points )
                , ( "net", encodeNet net )
                ]


decodeSurfaceConductor : Decoder SurfaceConductor
decodeSurfaceConductor =
    Decode.field "type" Decode.string |> Decode.andThen decodeSurfaceConductorHelp


decodeSurfaceConductorHelp type_ =
    case type_ of
        "trace" ->
            Decode.map2
                Trace
                (Decode.field "tracePoints" (Decode.list decodeTracePoint))
                (Decode.field "net" decodeNet)

        "surface-pad" ->
            Decode.map4
                SurfacePad
                (Decode.field "pad" decodePad)
                (Decode.field "point" decodePoint)
                (Decode.field "width" Decode.float)
                (Decode.field "net" decodeNet)

        "zone" ->
            Decode.map2
                Zone
                (Decode.field "points" (Decode.list decodePoint))
                (Decode.field "net" decodeNet)

        other ->
            Decode.fail <| "Unknown type for type SurfaceConductor: " ++ other


encodeTracePoint : TracePoint -> Encode.Value
encodeTracePoint tp =
    Encode.object
        [ ( "point", encodePoint tp.point )
        , ( "thickness", Encode.float tp.thickness )
        ]


decodeTracePoint : Decoder TracePoint
decodeTracePoint =
    Decode.map2
        TracePoint
        (Decode.field "point" decodePoint)
        (Decode.field "thickness" Decode.float)


encodePad : Pad -> Encode.Value
encodePad pad =
    Encode.object
        [ ( "number", encodeMaybeInt pad.number )
        , ( "label", encodeMaybeLabel pad.label )
        ]


decodePad : Decoder Pad
decodePad =
    Decode.map2
        Pad
        (Decode.field "number" (Decode.maybe Decode.int))
        (Decode.field "label" (Decode.maybe decodePadLabel))


encodeMaybeLabel : Maybe PadLabel -> Encode.Value
encodeMaybeLabel mpl =
    case mpl of
        Nothing ->
            Encode.null

        Just pl ->
            Encode.object
                [ ( "text", Encode.string pl.text )
                , ( "rotation", Encode.float pl.rotation )
                ]


decodePadLabel : Decoder PadLabel
decodePadLabel =
    Decode.map2
        PadLabel
        (Decode.field "text" Decode.string)
        (Decode.field "rotation" Decode.float)


encodeMaybeInt : Maybe Int -> Encode.Value
encodeMaybeInt mi =
    Maybe.withDefault Encode.null (Maybe.map Encode.int mi)


encodeNet : Net -> Encode.Value
encodeNet net =
    case net of
        NoNet i ->
            Encode.object
                [ ( "type", Encode.string "no-net" )
                , ( "id", Encode.int i )
                ]

        AutoNet i ->
            Encode.object
                [ ( "type", Encode.string "auto-net" )
                , ( "id", Encode.int i )
                ]

        CustomNet name color ->
            Encode.object
                [ ( "type", Encode.string "custom-net" )
                , ( "name", Encode.string name )
                , ( "color", Encode.string color )
                ]


decodeNet : Decoder Net
decodeNet =
    Decode.field "type" Decode.string |> Decode.andThen decodeNetHelp


decodeNetHelp type_ =
    case type_ of
        "no-net" ->
            Decode.map
                NoNet
                (Decode.field "id" Decode.int)

        "auto-net" ->
            Decode.map
                AutoNet
                (Decode.field "id" Decode.int)

        "custom-net" ->
            Decode.map2
                CustomNet
                (Decode.field "name" Decode.string)
                (Decode.field "color" Decode.string)

        other ->
            Decode.fail <| "Unknown type for type Net: " ++ other


encodeDimension : Dimension -> Encode.Value
encodeDimension dimension =
    case dimension of
        DistanceDimension p1 p2 ->
            Encode.object
                [ ( "type", Encode.string "distance" )
                , ( "p1", encodePoint p1 )
                , ( "p2", encodePoint p2 )
                ]

        AngleDimension p1 p2 p3 ->
            Encode.object
                [ ( "type", Encode.string "angle" )
                , ( "p1", encodePoint p1 )
                , ( "p2", encodePoint p2 )
                , ( "p3", encodePoint p3 )
                ]


decodeDimension : Decoder Dimension
decodeDimension =
    Decode.field "type" Decode.string |> Decode.andThen decodeDimensionHelp


decodeDimensionHelp type_ =
    case type_ of
        "distance" ->
            Decode.map2
                DistanceDimension
                (Decode.field "p1" decodePoint)
                (Decode.field "p2" decodePoint)

        "angle" ->
            Decode.map3
                AngleDimension
                (Decode.field "p1" decodePoint)
                (Decode.field "p2" decodePoint)
                (Decode.field "p3" decodePoint)

        other ->
            Decode.fail <| "Unknown type for type Dimension: " ++ other
