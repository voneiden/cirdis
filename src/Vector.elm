module Vector exposing (generateDoubleRow, generateSingleRow)

import Common exposing (Pad, Point)


vectorSum : Point -> Point -> Point
vectorSum p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }


vectorMultiply : Point -> Float -> Point
vectorMultiply p m =
    { x = p.x * m, y = p.y * m }


vectorDot : Point -> Point -> Float
vectorDot p1 p2 =
    p1.x * p2.x + p1.y * p2.y


vectorLength : Point -> Float
vectorLength p =
    sqrt (p.x ^ 2 + p.y ^ 2)


vectorProjection : Point -> Point -> Point -> ( Point, Point, Point )
vectorProjection p1 p2 p3 =
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
            vectorLength b

        -- Axis unit
        bu : Point
        bu =
            { x = b.x / bl, y = b.y / bl }

        -- Projection length
        pl =
            vectorDot a b / bl

        -- Projection
        p : Point
        p =
            { x = bu.x * pl, y = bu.y * pl }
    in
    ( a, b, p )


vectorRejection : Point -> Point -> Point
vectorRejection a p =
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
            vectorLength b

        -- Axis unit
        bu : Point
        bu =
            { x = b.x / bl, y = b.y / bl }

        -- Projection length
        pl =
            vectorDot a b / bl

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


generateSingleRow : Int -> Point -> Point -> Point -> List ( Point, Pad )
generateSingleRow startNumber p1 p2 p3 =
    let
        ( a, b, p ) =
            vectorProjection p1 p2 p3

        count =
            round <| vectorLength p / vectorLength b
    in
    List.map (\number -> ( vectorSum p1 (vectorMultiply b (toFloat number)), { number = Just (number + startNumber), label = Nothing } )) (List.range 0 (max 1 count))


generateDoubleRow : Point -> Point -> Point -> List ( Point, Pad )
generateDoubleRow p1 p2 p3 =
    let
        ( a, b, p ) =
            vectorProjection p1 p2 p3

        r =
            vectorRejection a p

        p1r =
            vectorSum p1 r

        count =
            max 2 <| round <| vectorLength p / vectorLength b

        total =
            count * 2
    in
    List.map (\number -> ( vectorSum p1 (vectorMultiply b (toFloat number)), { number = Just (number + 1), label = Nothing } )) (List.range 0 count)
        ++ List.map
            (\number ->
                ( vectorSum p1r (vectorMultiply b (toFloat number))
                , { number = Just (total - number + 2), label = Nothing }
                )
            )
            (List.range 0 count)
