module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App as App
import Html.Attributes exposing (href, class, style)
import Random exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options exposing (css)
import Maze exposing (Maze, Cell)


type alias Mdl =
    Material.Model


type alias Model =
    { maze : Maze
    , mdl : Material.Model
    }


model =
    { maze = Maze.grid
    , mdl = Material.model
    }


type Msg
    = GenerateRandomMaze
    | RotateRight
    | NewMaze (List (List ( Int, Int )))
    | Mdl (Material.Msg Msg)


generator : Generator (List (List (Int, Int)))
generator =
    list 10 <| list 10 <| pair (int 0 3) (int 0 3)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMaze randomInit ->
            ( {model | maze = Maze.generateMaze randomInit}, Cmd.none )

        GenerateRandomMaze ->
            ( model, Random.generate NewMaze generator)

        RotateRight ->
            ( model, Cmd.none )

        Mdl msg' ->
            Material.update msg' model


view : Model -> Html Msg
view model =
    div [ style [ ( "padding", "3rem" ) ] ]
        [ Maze.view model.maze
        , Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.onClick GenerateRandomMaze, css "marging" "0 24px" ]
            [ text "Generate Random Maze" ]
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Button.onClick RotateRight ]
            [ text "RotateRight" ]
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
