module PathFinder exposing (pathFinder)

import Maze exposing (Maze, Tile(..), blank)
import Set exposing (Set)
import Dict exposing (Dict)


fromJust : Maybe a -> a
fromJust just =
    case just of
        Nothing ->
            Debug.crash "did you just do a fromJust on Nothing???"

        Just a ->
            a


type alias X =
    Int


type alias Y =
    Int


type alias Position =
    ( X, Y )


type alias PositionedTile =
    ( Position, Tile )


positionedTile : Position -> Maze -> PositionedTile
positionedTile pos maze =
    ( pos, Maybe.withDefault blank <| Dict.get pos maze )


connected : PositionedTile -> PositionedTile -> Bool
connected ( ( x1, y1 ), tile1 ) ( ( x2, y2 ), tile2 ) =
    case ( (x1 - x2), (y1 - y2), tile1, tile2 ) of
        ( 1, 0, Tile _ _ _ True, Tile _ True _ _ ) ->
            True

        ( -1, 0, Tile _ True _ _, Tile _ _ _ True ) ->
            True

        ( 0, 1, Tile True _ _ _, Tile _ _ True _ ) ->
            True

        ( 0, -1, Tile _ _ True _, Tile True _ _ _ ) ->
            True

        _ ->
            False


connectedNeighbours : Position -> Maze -> List Position
connectedNeighbours (( x, y ) as pos) maze =
    [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.map (\pos -> positionedTile pos maze)
        |> List.filter (connected (positionedTile pos maze))
        |> List.map fst


pathFinder : Position -> Position -> Maze -> List Position
pathFinder from to maze =
    let
        initialConnectedNeighbours =
            Set.fromList <| connectedNeighbours from maze
    in
        pathFinder' from from to maze initialConnectedNeighbours (Set.singleton from) []


pathFinder' : Position -> Position -> Position -> Maze -> Set Position -> Set Position -> List Position -> List Position
pathFinder' origFrom from to maze toVisit visited path =
    case Set.size toVisit of
        0 ->
            []

        _ ->
            case Set.member to toVisit of
                True ->
                    to :: path

                False ->
                    let
                        distance (x1, y1) (x2, y2) =
                            (abs <| x1 - x2) + (abs <| y1 - y2)
                        next =
                            toVisit
                                |> Set.toList
                                |> List.map (\pos -> (pos, distance origFrom pos))
                                |> List.sortBy snd
                                |> List.head
                                |> fromJust
                                |> fst

                        markedAsVisited =
                            Set.insert next visited

                        allConnected =
                            connectedNeighbours next maze
                                |> Set.fromList
                                |> Set.union toVisit
                                |> \temp -> Set.diff temp markedAsVisited
                    in
                        pathFinder' origFrom next to maze allConnected markedAsVisited (from :: path)
