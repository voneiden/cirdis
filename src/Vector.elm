module Vector exposing (..)

import Common exposing (Pad, Point)


sum : Point -> Point -> Point
sum p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }


sub : Point -> Point -> Point
sub p1 p2 =
    { x = p1.x - p2.x, y = p1.y - p2.y }


mul : Point -> Float -> Point
mul p m =
    { x = p.x * m, y = p.y * m }


dot : Point -> Point -> Float
dot p1 p2 =
    p1.x * p2.x + p1.y * p2.y


len : Point -> Float
len p =
    sqrt (p.x ^ 2 + p.y ^ 2)


unit : Point -> Float -> Point
unit p length =
    { x = p.x / length, y = p.y / length }


rot90cw : Point -> Point
rot90cw p =
    { x = -p.y, y = p.x }


rot90ccw : Point -> Point
rot90ccw p =
    { x = p.y, y = -p.x }


projection : Point -> Point -> Point -> ( Point, Point, Point )
projection p1 p2 p3 =
    let
        -- Opposite vector a
        a : Point
        a =
            { x = p3.x - p1.x, y = p3.y - p1.y }

        -- Axis vector b
        b : Point
        b =
            { x = p2.x - p1.x, y = p2.y - p1.y }

        -- Axis length
        bl =
            len b

        -- Axis unit
        bu : Point
        bu =
            { x = b.x / bl, y = b.y / bl }

        -- Projection length
        pl =
            dot a b / bl

        -- Projection
        p : Point
        p =
            { x = bu.x * pl, y = bu.y * pl }
    in
    ( a, b, p )


rejection : Point -> Point -> Point
rejection a p =
    { x = a.x - p.x, y = a.y - p.y }


generateProjectionRejection : Point -> Point -> Point -> ( Point, Point )
generateProjectionRejection p1 p2 p3 =
    let
        -- Opposite vector a
        a : Point
        a =
            { x = p3.x - p1.x, y = p3.y - p1.y }

        -- Axis vector b
        b : Point
        b =
            { x = p2.x - p1.x, y = p2.y - p1.y }

        -- Axis length
        bl =
            len b

        -- Axis unit
        bu : Point
        bu =
            { x = b.x / bl, y = b.y / bl }

        -- Projection length
        pl =
            dot a b / bl

        -- Projection
        p : Point
        p =
            { x = bu.x * pl, y = bu.y * pl }

        -- Axis normal
        bn =
            { x = -b.y, y = b.x }

        -- Axis normal unit
        bnu =
            { x = -bu.y, y = bu.x }

        -- Rejection
        r =
            { x = a.x - p.x, y = a.y - p.y }
    in
    ( p, r )


{-| Caclulate the row step vector and number of projections that completes the row
-}
calculateRowStepVector : Float -> Point -> Point -> Point -> ( Point, Int )
calculateRowStepVector minSpacing p1 p2 p3 =
    let
        vt =
            sub p2 p1

        lt =
            len vt

        ut =
            unit vt lt

        -- density
        vd =
            sub p3 p2

        ld =
            len vd

        targetSpacing =
            max minSpacing ld

        count =
            round <| lt / targetSpacing

        spacing =
            lt / toFloat count

        -- step vector
        vs =
            mul ut spacing
    in
    ( vs, count )


generateSingleRow : Int -> Float -> Point -> Point -> Point -> List ( Point, Pad )
generateSingleRow startNumber minSpacing p1 p2 p3 =
    let
        ( vs, count ) =
            calculateRowStepVector minSpacing p1 p2 p3
    in
    List.map (\number -> ( sum p1 (mul vs (toFloat number)), { number = Just (number + startNumber), label = Nothing } )) (List.range 0 (max 1 count))


generateDoubleRow : Float -> Point -> Point -> Point -> Point -> List ( Point, Pad )
generateDoubleRow minSpacing p1 p2 p3 p4 =
    -- p1, p2 and p3 behave like single row tool
    -- p4 determines the spacing between the rows
    let
        ( vs, count ) =
            calculateRowStepVector minSpacing p1 p2 p3

        total =
            count * 2

        ( a, _, p ) =
            projection p1 (sum p1 vs) p4

        r =
            rejection a p

        p1r =
            sum p1 r
    in
    List.map (\number -> ( sum p1 (mul vs (toFloat number)), { number = Just (number + 1), label = Nothing } )) (List.range 0 count)
        ++ List.map
            (\number ->
                ( sum p1r (mul vs (toFloat number))
                , { number = Just (total - number + 2), label = Nothing }
                )
            )
            (List.range 0 count)
