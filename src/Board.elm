module Board exposing (view, Model, update, empty, initBoardMsg, updatePathMsg, Msg)

import Html exposing (Html, div, text, span)
import Maze exposing (Maze, Tile(..))
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


type alias Path =
    List Position


type alias Model =
    { maze : Maze
    , playingTile : Tile
    , path : Path
    }


empty : Model
empty =
    { maze = Dict.empty
    , playingTile = Maze.blank
    , path = []
    }


type Msg
    = MazeInitialized Maze Tile
    | PathUpdated Path


initBoardMsg : ( Maze, Tile ) -> Msg
initBoardMsg ( maze, tile ) =
    MazeInitialized maze tile


updatePathMsg : Path -> Msg
updatePathMsg path =
    PathUpdated path


update : Msg -> Model -> Model
update msg model =
    case msg of
        MazeInitialized maze tile ->
            { model | maze = maze, playingTile = tile }

        PathUpdated path ->
            { model | path = path }


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

        viewTile' ( ( x, y ), cell ) =
            viewTile ( x * 100, y * 100 ) cell
    in
        List.map viewTile' cells


rotateClockwise : Position -> Int -> List (Attribute msg)
rotateClockwise ( x, y ) nb =
    let
        degrees =
            nb % 4 |> (*) 90 |> toString
    in
        [ transform ("translate (" ++ (toString <| x) ++ ", " ++ (toString <| y) ++ ")" ++ "rotate(" ++ degrees ++ ",50,50)") ]


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


viewTile : Position -> Tile -> Html msg
viewTile pos cell =
    let
        square color =
            Svg.path [ d "M 1 1 H 99 V 99 H 1", fill color ] []

        blank =
            [ square "gray" ]

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
            Tile True False True False ->
                g (rotateClockwise pos 0) straight

            Tile False True False True ->
                g (rotateClockwise pos 1) straight

            -- tee
            Tile True True True False ->
                g (rotateClockwise pos 0) tee

            Tile False True True True ->
                g (rotateClockwise pos 1) tee

            Tile True False True True ->
                g (rotateClockwise pos 2) tee

            Tile True True False True ->
                g (rotateClockwise pos 3) tee

            -- elbow
            Tile True True False False ->
                g (rotateClockwise pos 0) elbow

            Tile False True True False ->
                g (rotateClockwise pos 1) elbow

            Tile False False True True ->
                g (rotateClockwise pos 2) elbow

            Tile True False False True ->
                g (rotateClockwise pos 3) elbow

            -- blank
            _ ->
                g (rotateClockwise pos 0) blank
