module SvgMaze exposing (view)

import Html exposing (Html, div, text, span)
import Html.App as App
import Html.Attributes exposing (href, class, style)
import Maze exposing (Maze, Cell(..))
import Svg exposing (svg, use, Attribute)
import Svg.Attributes exposing (x, y, xlinkHref, transform, width, height)
import Dict


type alias Position =
    ( Int, Int )


view : Maze -> Html msg
view maze =
    let
        cells =
            Dict.toList maze

        viewCell' ( ( x, y ), cell ) =
            viewCell ( y * 100, x * 100 ) cell
    in
        svg [ width "1000", height "1000" ] (List.map viewCell' cells)


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
