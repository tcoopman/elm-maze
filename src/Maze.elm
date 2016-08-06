module Maze exposing (Maze, Cell(..), grid, generator)

import Html exposing (text, Html, div, span)
import Html.Attributes exposing (style)
import Dict exposing (Dict)
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


type alias Position =
    ( Int, Int )


type alias Maze =
    Dict Position Cell


grid =
    buildMaze [ [ ( 1, 2 ), ( 0, 1 ) ], [ ( 1, 1 ), ( 3, 1 ) ] ]


{-| generates a random cell, with (type, rotation)

type should be a random from 0 to 3
rotation should be a random int from 0 to 3
-}
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
    let
        listMaze =
            List.map (List.map buildCell) randomInit

        withX x rows =
            List.indexedMap (withY x) rows

        withY x y cell =
            ( ( x, y ), cell )

        dictList =
            List.indexedMap withX listMaze
    in
        Dict.fromList (List.concat dictList)


generator : Int -> Random.Generator Maze
generator size =
    let
        randomInit =
            Random.list size <| Random.list size <| Random.pair (Random.int 0 3) (Random.int 0 3)
    in
        Random.map buildMaze randomInit
