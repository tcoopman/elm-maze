module SvgMaze exposing (view, Model, update, init)

import Html exposing (Html, div, text, span)
import Maze exposing (Maze, Cell(..))
import Svg exposing (svg, use, Attribute, g)
import Svg.Attributes exposing (x, y, xlinkHref, transform, width, height, d, stroke, x1, x2, y1, y2)
import Dict


{-| helper function to pair items of a list
-}
pair : List a -> List ( a, a )
pair xs =
    let
        pair' next ( result, maybePrevious ) =
            case maybePrevious of
                Nothing ->
                    ( result, Just next )

                Just previous ->
                    ( ( previous, next ) :: result, Just next )
    in
        fst (List.foldl pair' ( [], Nothing ) xs)


type alias Position =
    ( Int, Int )


type alias Model =
    { maze : Maze
    , path : List Position
    }


init =
    { maze = Dict.empty
    , path = [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ) ]
    }


update : Maze -> Model -> Model
update maze model =
    { model | maze = maze }


view : Model -> Html msg
view model =
    svg [ width "1000", height "1000" ]
        [ g [] (viewMaze model.maze)
        , g [] (viewPath model.path)
        ]


viewMaze : Maze -> List (Html msg)
viewMaze maze =
    let
        cells =
            Dict.toList maze

        viewCell' ( ( x, y ), cell ) =
            viewCell ( y * 100, x * 100 ) cell
    in
        List.map viewCell' cells


rotateClockwise : Position -> Int -> Attribute msg
rotateClockwise ( x, y ) nb =
    let
        degrees =
            case nb % 4 of
                0 ->
                    "0"

                1 ->
                    "90"

                2 ->
                    "180"

                3 ->
                    "270"

                _ ->
                    Debug.crash "cannot happen (mod 4)"

        rotTranslate i =
            i + 50 |> toString
    in
        transform ("rotate(" ++ degrees ++ ", " ++ rotTranslate x ++ ", " ++ rotTranslate y ++ ")")


viewPath : List Position -> List (Html msg)
viewPath positions =
    let
        svgPos i =
            i * 100 + 50 |> toString

        viewPath' ( ( x1', y1' ), ( x2', y2' ) ) =
            Svg.line
                [ x1 <| svgPos x1'
                , x2 <| svgPos x2'
                , y1 <| svgPos y1'
                , y2 <| svgPos y2'
                , stroke "blue"
                ]
                []
    in
        pair positions |> List.map viewPath'


viewCell : Position -> Cell -> Html msg
viewCell pos cell =
    let
        baseAttributes =
            [ x (toString <| fst pos)
            , y (toString <| snd pos)
            , width "100"
            , height "100"
            ]

        ref =
            case cell of
                Cell True False True False ->
                    [ xlinkHref "#straight" ] ++ baseAttributes

                Cell False True False True ->
                    [ xlinkHref "#straight", rotateClockwise pos 1 ] ++ baseAttributes

                Cell True True True False ->
                    [ xlinkHref "#tee" ] ++ baseAttributes

                Cell False True True True ->
                    [ xlinkHref "#tee", rotateClockwise pos 1 ] ++ baseAttributes

                Cell True False True True ->
                    [ xlinkHref "#tee", rotateClockwise pos 2 ] ++ baseAttributes

                Cell True True False True ->
                    [ xlinkHref "#tee", rotateClockwise pos 3 ] ++ baseAttributes

                Cell False True True False ->
                    [ xlinkHref "#elbow" ] ++ baseAttributes

                Cell False False True True ->
                    [ xlinkHref "#elbow", rotateClockwise pos 1 ] ++ baseAttributes

                Cell True False False True ->
                    [ xlinkHref "#elbow", rotateClockwise pos 2 ] ++ baseAttributes

                Cell True True False False ->
                    [ xlinkHref "#elbow", rotateClockwise pos 3 ] ++ baseAttributes

                _ ->
                    [ xlinkHref "#blank" ] ++ baseAttributes
    in
        use ref []
