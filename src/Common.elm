module Common exposing (..)

-- Types


type alias NetId =
    Int


type alias Radius =
    Float


type alias Thickness =
    Float


type alias Width =
    Float


type alias Dragging =
    Bool


type alias ShiftPressed =
    Bool


type alias Point =
    { x : Float
    , y : Float
    }


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
