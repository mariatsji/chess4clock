-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html, div, p, text, button)
import Time exposing (Time, second)
import Html.Events exposing (onClick)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type Player = A | B | C | D

type alias Clocks =
  { a : Int
  , b : Int
  , c : Int
  , d : Int
  , active : Maybe Player }

type alias Model = Clocks

startingTime =
  { a = 300
  , b = 300
  , c = 300
  , d = 300
  , active = Nothing }

init : (Model, Cmd Msg)
init =
  (startingTime, Cmd.none)

-- UPDATE

type Msg
  = Switch Player | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg clocks =
  case msg of
    Switch newPlayer ->
      (newTime newPlayer clocks, Cmd.none)
    Tick _ ->
      case clocks.active of
          Nothing ->
            (clocks, Cmd.none)
          Just p ->
            (newTime p clocks, Cmd.none)

newTime : Player -> Model -> Model
newTime p m =
  case p of
    A -> { m | a  = m.a - 1
             , active = Just A }
    B -> { m | b  = m.b - 1
             , active = Just B }
    C -> { m | c  = m.c - 1
             , active = Just C }
    D -> { m | d  = m.d - 1
             , active = Just D }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick


-- VIEW

view : Model -> Html Msg
view clocks =
    div [ ]
      [ p [ ]
        [ text (toString clocks.active) ]
      , div [ ]
        [ text (toString clocks.a)
        , button [ onClick ( Switch A) ] [ text "GO" ]
        ]
      , div [ ]
        [ text (toString clocks.b)
        , button [ onClick ( Switch B) ] [ text "GO" ]
        ]
      , div [ ]
        [ text (toString clocks.c)
        , button [ onClick ( Switch C) ] [ text "GO" ]
        ]
      , div [ ]
        [ text (toString clocks.d)
        , button [ onClick ( Switch D) ] [ text "GO" ]
        ]
    ]
