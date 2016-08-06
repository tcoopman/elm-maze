module PathFinder exposing (findCell)

import Maze exposing (Maze, Cell)


type alias X =
    Int


type alias Y =
    Int


type alias Position =
    ( X, Y )


connectedH : Cell -> Cell -> Bool
connectedH left right =
    case ( left, right ) of
        ( Cell _ True _ _, Cell _ _ _ True ) ->
            True

        _ ->
            False


connectedV : Cell -> Cell -> Bool
connectedV up down =
    connectedH (rotate down) (rotate up)


pathFinder : Position -> Position -> Maze -> Bool
pathFinder from to grid =
    pathFinder' from to grid (connectedNeighbours from grid) Set.empty


pathFinder' : Position -> Position -> Maze -> List Position -> Set Position -> Bool
pathFinder' from to grid toVisit visited =
    True


connectedNeighbours : Position -> Maze -> List Position
connectedNeighbours position grid =
    []


findCell : Position -> Maze -> Cell
findCell ( x, y ) grid =
    let
        row =
            Maybe.withDefault [] (List.head (List.drop y grid))
    in
        Maybe.withDefault empty (List.head (List.drop x row))
