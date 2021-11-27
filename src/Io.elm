module Io exposing (..)

-- Main model

import Common exposing (Dimension(..), Pad, PadLabel, Point, ReferenceFrame)
import Conductor exposing (Net(..), SurfaceConductor(..), ThroughConductor(..), TracePoint)
import Dict exposing (Dict)
import Json.Encode as Encode
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import VirtualDom
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
    , zPressed : Bool
    , xPressed : Bool
    , vPressed : Bool
    , includeSource : Bool
    }


type alias LayerData =
    { title : String
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
        , ( "layers", Encode.list encodeLayerData (Dict.toList model.layers) ) -- TODO duplicate data!
        , ( "workspace", encodeWorkspace model.timeline.current )
        ]


encodeLayerData : ( Int, LayerData ) -> Encode.Value
encodeLayerData ( id, layerData ) =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "title", Encode.string layerData.title )
        , ( "mimeType", Encode.string layerData.mimeType )

        --, ( "b64Data", Encode.string layerData.b64Data )
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
        , ( "ref", encodeMaybeReference ws.ref )
        , ( "dimensions", Encode.list encodeDimension ws.dimensions )
        ]


encodeLayer : Workspace.Layer -> Encode.Value
encodeLayer layer =
    Encode.object
        [ ( "id", Encode.int layer.id )
        , ( "opacity", Encode.int layer.opacity )
        , ( "conductors", Encode.list encodeSurfaceConductor layer.conductors )
        ]


encodeTransform : Workspace.Transform -> Encode.Value
encodeTransform transform =
    Encode.object
        [ ( "x", Encode.float transform.x )
        , ( "y", Encode.float transform.y )
        , ( "z", Encode.float transform.z )
        ]


encodeMaybeReference : Maybe ReferenceFrame -> Encode.Value
encodeMaybeReference mrf =
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


encodePoint : Point -> Encode.Value
encodePoint point =
    Encode.object
        [ ( "x", Encode.float point.x )
        , ( "y", Encode.float point.y )
        ]


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


encodeTracePoint : TracePoint -> Encode.Value
encodeTracePoint tp =
    Encode.object
        [ ( "point", encodePoint tp.point )
        , ( "thickness", Encode.float tp.thickness )
        ]


encodePad : Pad -> Encode.Value
encodePad pad =
    Encode.object
        [ ( "number", encodeMaybeInt pad.number )
        , ( "label", encodeMaybeLabel pad.label )
        ]


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
