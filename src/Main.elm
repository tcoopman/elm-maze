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
    , size : Float
    , mdl : Material.Model
    }


model =
    { board = Board.init
    , size = 5
    , mdl = Material.model
    }


type Msg
    = GenerateRandomMaze
    | NewMaze Maze
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMaze randomMaze ->
            ( { model | board = Board.update (Board.updateMaze randomMaze) model.board }, Cmd.none )

        GenerateRandomMaze ->
            ( model, Random.generate NewMaze (Maze.generator (round model.size)) )

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
                [ text <| toString <| pathFinder ( 0, 0 ) ( 1, 0 ) model.board.maze ]
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
