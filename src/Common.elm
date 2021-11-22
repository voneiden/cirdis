module Common exposing (..)

-- Types


type alias NetId =
    Int


type alias Radius =
    Float


type alias Color =
    String


type alias Hidden =
    Bool


type alias Thickness =
    Float


type alias Width =
    Float


type alias Dragging =
    Bool


type alias ShiftPressed =
    Bool


type alias CtrlPressed =
    Bool


type alias Point =
    { x : Float
    , y : Float
    }


type alias Pad =
    { number : Maybe Int
    , label : Maybe PadLabel
    }


type alias PadLabel =
    { text : String
    , rotation : Float
    }


type alias ReferenceFrame =
    { p1 : Point
    , p2 : Point
    , value : Float
    , unit : String
    }

type Dimension
    = DistanceDimension Point Point
    | AngleDimension Point Point Point

chainUpdate : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
chainUpdate toNew ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            toNew model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )

chainUpdate3 : (model -> ( model, Cmd msg, Bool )) -> ( model, Cmd msg, Bool ) -> ( model, Cmd msg, Bool )
chainUpdate3 toNew ( model, cmd, updateTimeline ) =
    let
        ( newModel, newCmd, newUpdateTimeline ) =
            toNew model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ], updateTimeline || newUpdateTimeline )



{-| Svg helper for converting points into SVG coordinate strings (like: M 5,5)
-}
fromPoint : String -> Point -> String
fromPoint cmd point =
    cmd ++ " " ++ String.fromFloat point.x ++ "," ++ String.fromFloat point.y


{-| Distance between two points
-}
distanceToPoint : Point -> Point -> Float
distanceToPoint p1 p2 =
    sqrt ((p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2)



-- Functions


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


{-| Reduce a list to unique items
-}
unique : List a -> List a
unique list =
    List.foldl
        (\a uniques ->
            if List.member a uniques then
                uniques

            else
                uniques ++ [ a ]
        )
        []
        list


{-| Take a list of things (points for example) and return them as chained pairs

p1, p2, p3, p4 --> (p1, p2), (p2, p3), (p3, p4)

-}
toPairs : List a -> List ( a, a )
toPairs elements =
    case elements of
        p1 :: p2 :: rest ->
            ( p1, p2 ) :: toPairs (p2 :: rest)

        _ ->
            []
