module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App as App
import Html.Attributes exposing (href, class, style)
import Random exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Icon as Icon
import Material.Grid as Grid
import Material.Options exposing (css)
import Maze exposing (Maze, Tile)
import PathFinder exposing (pathFinder)
import Board


type alias Mdl =
    Material.Model


type alias Model =
    { board : Board.Model
    , mdl : Material.Model
    }


model =
    { board = Board.empty
    , mdl = Material.model
    }


type Msg
    = GenerateRandomMaze
    | NewGame ( Maze, Tile )
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ( maze, tile ) ->
            ( { model | board = Board.update (Board.initBoardMsg ( maze, tile )) model.board }, Cmd.none )

        GenerateRandomMaze ->
            ( model, Random.generate NewGame Maze.generator )

        Mdl msg' ->
            Material.update msg' model


view : Model -> Html Msg
view model =
    div [ style [ ( "padding", "3rem" ) ] ]
        [ Grid.grid []
            [ Grid.cell [ Grid.size Grid.All 1 ]
                [ Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.onClick GenerateRandomMaze
                    , css "marging" "0 24px"
                    , Button.icon
                    , Button.colored
                    ]
                    [ Icon.i "refresh" ]
                ]
            , Grid.cell [ Grid.size Grid.All 2 ]
                [ text <| toString <| pathFinder ( 0, 0 ) ( 3, 3 ) model.board.maze ]
            , Grid.cell [ Grid.size Grid.All 12 ]
                [ Board.view model.board ]
            ]
        ]
        |> Material.Scheme.top


main : Program Never
main =
    App.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
