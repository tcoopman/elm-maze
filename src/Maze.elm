module Maze exposing (Maze, Tile(..), generator, getTile, blank)

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


type Tile
    = Tile North East South West


{-| ┌ ┐ ┘ └
-}
elbow : Tile
elbow =
    Tile True True False False


{-| ┴ ├  ┬ ┤
-}
tee : Tile
tee =
    Tile True True False True


{-| rotateN 0 -> │ or rotateN 1 -> ─
-}
straight : Tile
straight =
    Tile True False True False


blank : Tile
blank =
    Tile False False False False


rotate : Tile -> Tile
rotate inputTile =
    case inputTile of
        Tile a b c d ->
            Tile d a b c


rotateN : Int -> Tile -> Tile
rotateN n inputTile =
    if n > 0 then
        rotateN (n - 1) (rotate inputTile)
    else
        inputTile


type alias Position =
    ( Int, Int )


type alias Maze =
    Dict Position Tile


getTile : Position -> Maze -> Tile
getTile pos maze =
    Maybe.withDefault blank (Dict.get pos maze)


{-| Generates a random maze.

A maze has a very specific build:

* A board is 7 by 7 (so 49 tiles)
* 16 tiles are *fixed*
* The game has 12 straight, 15 elbow and 7 tee tiles
* This is a total of 50. The extra tile is the starting tile.
-}
generator : Random.Generator ( Maze, Tile )
generator =
    let
        fixedTiles =
            [ (rotateN 1 elbow)
            , (rotateN 2 tee)
            , (rotateN 2 tee)
            , (rotateN 2 elbow)
            , (rotateN 1 tee)
            , (rotateN 1 tee)
            , (rotateN 2 tee)
            , (rotateN 3 tee)
            , (rotateN 1 tee)
            , (rotateN 0 tee)
            , (rotateN 3 tee)
            , (rotateN 3 tee)
            , (rotateN 0 elbow)
            , (rotateN 0 tee)
            , (rotateN 0 tee)
            , (rotateN 3 elbow)
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


buildMaze' : List (List Tile) -> Maze
buildMaze' cells =
    let
        withY y rows =
            List.indexedMap (withX y) rows

        withX y x cell =
            ( ( x, y ), cell )

        dictList =
            List.indexedMap withY cells
    in
        Dict.fromList <| List.concat dictList
