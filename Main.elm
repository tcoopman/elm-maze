module Main exposing (..)

import Html exposing (Html)
import Html.App as Html

import Maze exposing (Maze, Cell)


type alias Model =
    { maze : Maze
    }

model = { maze = Maze.grid }

type Msg = RotateLeft | RotateRight

update : Msg -> Model -> Model
update msg model =
  case msg of
    RotateLeft ->
      model
    RotateRight ->
      model

view : Model -> Html msg
view model =
  Maze.view model.maze

main =
  Html.beginnerProgram { model = model, view = view, update = update }
