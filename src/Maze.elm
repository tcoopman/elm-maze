module Maze exposing (Maze, Cell, grid, view, generator)

import Html exposing (text, Html, div, span)
import Html.Attributes exposing (style)
import Random


type alias North =
    Bool


type alias East =
    Bool


type alias South =
    Bool


type alias West =
    Bool


type Cell
    = Cell North East South West


elbow : Cell
elbow =
    Cell True True False False


tee : Cell
tee =
    Cell True True False True


straight : Cell
straight =
    Cell True False True False


empty =
    Cell False False False False


rotate : Cell -> Cell
rotate inputCell =
    case inputCell of
        Cell a b c d ->
            Cell d a b c


rotateN : Int -> Cell -> Cell
rotateN n inputCell =
    if n > 0 then
        rotateN (n - 1) (rotate inputCell)
    else
        inputCell


type alias Maze =
    List (List Cell)


view : Maze -> Html msg
view maze =
    div [] (List.map drawLine maze)


drawLine l =
    div
        [ style [ ( "display", "flex" ) ]
        ]
        (List.map viewCell l)


viewCell : Cell -> Html msg
viewCell cell =
    span
        [ style
            [ ( "font-size", "6rem" )
            , ( "line-height", "6rem" )
            , ( "display", "flex" )
            , ( "margin", "0" )
            , ( "padding", "0" )
            , ( "align-content", "center" )
            ]
        ]
        [ text <| showCell cell ]


showCell : Cell -> String
showCell inputCell =
    case inputCell of
        Cell True False True False ->
            "│"

        Cell False True False True ->
            "─"

        Cell True True True False ->
            "├"

        Cell True False True True ->
            "┤"

        Cell False True True True ->
            "┬"

        Cell True True False True ->
            "┴"

        Cell False True True False ->
            "┌"

        Cell False False True True ->
            "┐"

        Cell True True False False ->
            "└"

        Cell True False False True ->
            "┘"

        _ ->
            " "



-- text (toString <| connectedV straight straight)


grid =
    buildMaze [ [ ( 1, 2 ), ( 0, 1 ) ], [ ( 1, 1 ), ( 3, 1 ) ] ]



-- generates a random cell, with (type, rotation)
-- type should be a random from 0 to 3
-- rotation should be a random int from 0 to 3


buildCell : ( Int, Int ) -> Cell
buildCell ( cellType, rotation ) =
    let
        cell =
            case (cellType % 4) of
                0 ->
                    empty

                1 ->
                    elbow

                2 ->
                    straight

                3 ->
                    tee

                _ ->
                    Debug.crash "This can never happen (% 4)"
    in
        rotateN rotation cell


buildMaze : List (List ( Int, Int )) -> Maze
buildMaze randomInit =
    List.map (List.map buildCell) randomInit


generator : Int -> Random.Generator Maze
generator size =
    let
        randomInit =
            Random.list size <| Random.list size <| Random.pair (Random.int 0 3) (Random.int 0 3)
    in
        Random.map buildMaze randomInit
