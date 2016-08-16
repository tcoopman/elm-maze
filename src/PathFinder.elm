module PathFinder exposing (pathFinder)

import Maze exposing (Maze, Tile(..))
import Set exposing (Set)
import Dict exposing (Dict)


type alias X =
    Int


type alias Y =
    Int


type alias Position =
    ( X, Y )


connectedH : Tile -> Tile -> Bool
connectedH left right =
    case ( left, right ) of
        ( Tile _ True _ _, Tile _ _ _ True ) ->
            True

        _ ->
            False


connectedV : Tile -> Tile -> Bool
connectedV up down =
    case ( up, down ) of
        ( Tile _ _ True _, Tile True _ _ _ ) ->
            True

        _ ->
            False


pathFinder : Position -> Position -> Maze -> Bool
pathFinder from to grid =
    pathFinder' from to grid (connectedNeighbours from grid) Set.empty


pathFinder' : Position -> Position -> Maze -> List Position -> Set Position -> Bool
pathFinder' from to grid toVisit visited =
    False


type alias A =
    { closedSet :
        Set Position
        -- nodes already evaluated
    , openSet :
        Set Position
        -- still to be evaluated
    , cameFrom :
        Dict Position Position
        -- for each node, what node can be most efficiently reached
    , gScore :
        Dict Position Int
        -- for each node, the cost from start to that node
    , fScore :
        Dict Position Int
        -- for each node, the cost from start to the goal, passing this node
    , result : List Position
    }


aStar : Position -> Position -> Maze -> A -> List Position
aStar start goal maze a =
    let
        openList =
            Set.toList a.openSet

        lowestFScoreInOpenSet =
            let
                score position ( fScore, bestPos ) =
                    case (Dict.get position a.fScore) of
                        Nothing ->
                            ( fScore, bestPos )

                        Just a ->
                            if a < fScore then
                                ( a, Just position )
                            else
                                ( fScore, bestPos )
            in
                case (List.foldr score ( 99999, Nothing ) openList) of
                    ( _, Nothing ) ->
                        Debug.crash "There should always be an elment"

                    ( _, Just current ) ->
                        current

        reconstructPath cameFrom current totalPath =
            []
    in
        case openList of
            [] ->
                a.result

            _ ->
                let
                    current =
                        lowestFScoreInOpenSet
                in
                    if current == goal then
                        reconstructPath a.cameFrom current []
                    else
                        []


connectedNeighbours : Position -> Maze -> List Position
connectedNeighbours position grid =
    []
