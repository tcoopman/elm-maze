module Maze exposing (Maze, Cell(..), generator, getCell)

import Dict exposing (Dict)
import Random
import Random.Array exposing (shuffle)
import Array


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


{-| ┌ ┐ ┘ └
-}
elbow : Cell
elbow =
    Cell True True False False


{-| ┴ ├  ┬ ┤
-}
tee : Cell
tee =
    Cell True True False True


{-| rotateN 0 -> │ or rotateN 1 -> ─
-}
straight : Cell
straight =
    Cell True False True False


blank : Cell
blank =
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


getCell : Position -> Maze -> Cell
getCell pos maze =
    Maybe.withDefault blank (Dict.get pos maze)


generator : Int -> Random.Generator Maze
generator size = Random.map fst generator'


{-| Generates a random maze.

A maze has a very specific build:

* A board is 7 by 7 (so 49 tiles)
* 16 tiles are *fixed*
* The game has 12 straight, 15 elbow and 7 tee tiles
* This is a total of 50. The extra tile is the starting tile.
-}
generator' : Random.Generator ( Maze, Cell )
generator' =
    let
        fixedTiles =
            [ (rotateN 2 elbow)
            , (rotateN 2 tee)
            , (rotateN 2 tee)
            , (rotateN 3 elbow)
            , (rotateN 1 tee)
            , (rotateN 1 tee)
            , (rotateN 2 tee)
            , (rotateN 3 tee)
            , (rotateN 1 tee)
            , (rotateN 0 tee)
            , (rotateN 3 tee)
            , (rotateN 3 tee)
            , (rotateN 1 elbow)
            , (rotateN 0 tee)
            , (rotateN 0 tee)
            , (rotateN 0 elbow)
            ]

        randomTilesGenerator =
            let
                tiles =
                    List.repeat 12 straight ++ List.repeat 15 elbow ++ List.repeat 7 tee

                permutated =
                    Random.map (Array.toList) (shuffle (Array.fromList tiles))

                randomRotations =
                    Random.list (List.length tiles) <| (Random.int 0 3)

                mapper l1 l2 =
                    case ( l1, l2 ) of
                        ( cell :: cells, rot :: rotations ) ->
                            (rotateN rot cell) :: (mapper cells rotations)

                        ( _, _ ) ->
                            []
            in
                Random.map2 mapper permutated randomRotations

        allTiles =
            let
                combine fixed random =
                    case ( fixed, random ) of
                        ( f1 :: f2 :: f3 :: f4 :: fixedRest, x :: y :: z :: randomRest ) ->
                            f1 :: x :: f2 :: y :: f3 :: z :: f4 :: (List.take 7 randomRest) ++ (combine fixedRest (List.drop 7 randomRest))
                        ( _, _ ) ->
                            []
            in
                Random.map (combine fixedTiles) randomTilesGenerator

        toMaze =
            let
                mazer allTiles =
                    [ List.take 7 allTiles
                    , List.take 7 (List.drop 7 allTiles)
                    , List.take 7 (List.drop 14 allTiles)
                    , List.take 7 (List.drop 21 allTiles)
                    , List.take 7 (List.drop 28 allTiles)
                    , List.take 7 (List.drop 35 allTiles)
                    , List.take 7 (List.drop 42 allTiles)
                    ]
                        |> buildMaze'
            in
                Random.map mazer allTiles
    in
        Random.map (\maze -> ( maze, elbow )) toMaze


buildMaze' : List (List Cell) -> Maze
buildMaze' cells =
    let
        withX x rows =
            List.indexedMap (withY x) rows

        withY x y cell =
            ( ( x, y ), cell )

        dictList =
            List.indexedMap withX cells
    in
        Dict.fromList (List.concat dictList)
