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
import Material.Slider as Slider
import Material.Options exposing (css)
import Maze exposing (Maze, Cell)


type alias Mdl =
    Material.Model


type alias Model =
    { maze : Maze
    , size : Float
    , mdl : Material.Model
    }


model =
    { maze = Maze.grid
    , size = 5
    , mdl = Material.model
    }


type Msg
    = GenerateRandomMaze
    | NewMaze Maze
    | UpdateMazeSize Float
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMaze randomMaze ->
            ( { model | maze = randomMaze }, Cmd.none )

        GenerateRandomMaze ->
            ( model, Random.generate NewMaze (Maze.generator (round model.size)) )

        UpdateMazeSize size' ->
            ( { model | size = size' }, Cmd.none )

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
            , Grid.cell [ Grid.size Grid.All 4 ]
                [ Slider.view
                    [ Slider.onChange UpdateMazeSize
                    , Slider.value model.size
                    , Slider.max 10
                    , Slider.min 1
                    , Slider.step 1
                    ]
                ]
            , Grid.cell [ Grid.size Grid.All 12 ]
                [ Maze.view model.maze ]
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
