module Board exposing (view, Model, update, init)

import Html exposing (Html, div, text, span)
import Maze exposing (Maze, Cell(..))
import Svg exposing (svg, use, Attribute, g)
import Svg.Attributes exposing (x, y, xlinkHref, transform, width, height, d, stroke, x1, x2, y1, y2, fill)
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


rotateClockwise : Position -> Int -> List (Attribute msg)
rotateClockwise ( x, y ) nb =
    let
        degrees = nb % 4 |> (*) 90 |> toString
    in
        [transform ("translate (" ++ (toString <| x) ++ ", " ++ (toString <| y) ++ ")" ++ "rotate(" ++ degrees ++ ",50,50)")]


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
        square color =
            Svg.path [ d "M 1 1 H 99 V 99 H 1", fill color ] []

        blank =
            [ square "gray"]

        straight =
            [ square "black"
            , Svg.path [ d "M 30 0 H 70 V 100 H 30", fill "green" ] []
            ]

        tee =
            [ square "black"
            , Svg.path [ d "M 30 0 H 70 V 30 H 100 V 70 H 70 V 100 H 30", fill "green" ] []
            ]

        elbow =
            [ square "black"
            , Svg.path [ d "M 30 0 H 70 V 30 H 100 V 70 H 30", fill "green" ] []
            ]
    in
        case cell of
            -- straight
            Cell True False True False ->
                g (rotateClockwise pos 0) straight

            Cell False True False True ->
                g (rotateClockwise pos 1) straight

            -- tee
            Cell True True True False ->
                g (rotateClockwise pos 0) tee

            Cell False True True True ->
                g (rotateClockwise pos 1) tee

            Cell True False True True ->
                g (rotateClockwise pos 2) tee

            Cell True True False True ->
                g (rotateClockwise pos 3) tee

            -- elbow
            Cell False True True False ->
                g (rotateClockwise pos 0) elbow

            Cell False False True True ->
                g (rotateClockwise pos 1) elbow

            Cell True False False True ->
                g (rotateClockwise pos 2) elbow

            Cell True True False False ->
                g (rotateClockwise pos 3) elbow

            -- blank
            _ ->
                g (rotateClockwise pos 0) blank
