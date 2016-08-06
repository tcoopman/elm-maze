module Maze exposing (Maze, Cell, grid, view)

import Html exposing (text, Html, div)


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
    div [] (List.map (text << showCell) l)


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
    [ [ elbow, (rotateN 2 elbow) ], [ elbow, elbow ] ]
