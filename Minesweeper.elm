module Minesweeper exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Html.Attributes exposing (class, id)
import Random exposing (Generator, int, pair)
import Json.Decode as Json


-- msg


type Msg
    = Guess Point
    | NewMine Point
    | NewFlag Point
    | Play Size
    | GameOver



-- generators


newPoint : Size -> Generator Point
newPoint size =
    pair (int 0 (upperLimit size)) (int 0 (upperLimit size))



-- model


type alias Model =
    { mines : List Point
    , flags : List Point
    , boardSize : Size
    , gameStatus : GameState
    }


type Size
    = Small
    | Medium
    | Large


type GameState
    = Playing
    | Waiting


type alias X =
    Int


type alias Y =
    Int


type alias Point =
    ( X, Y )


initialModel : Model
initialModel =
    { mines = []
    , flags = []
    , boardSize = Small
    , gameStatus = Waiting
    }


upperLimit : Size -> Int
upperLimit size =
    case size of
        Small ->
            15

        Medium ->
            30

        Large ->
            45


gameBoard : Size -> List Int
gameBoard size =
    List.range 0 (zeroed (upperLimit size))


zeroed : Int -> Int
zeroed num =
    --needed to zero-index the resulting lists
    num - 1


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMine m ->
            if List.length model.mines <= (numMines model.boardSize) then
                ( { model | mines = (m :: model.mines) }, Random.generate NewMine (newPoint model.boardSize) )
            else
                ( model, Cmd.none )

        NewFlag f ->
            ( { model | flags = (f :: model.flags) }, Cmd.none )

        Guess p ->
            if List.member p model.mines then
                update GameOver model
            else
                ( model, Cmd.none )

        Play s ->
            ( { model | gameStatus = Playing, boardSize = s }, Random.generate NewMine (newPoint s) )

        GameOver ->
            ( { model | gameStatus = Waiting }, Cmd.none )


numMines : Size -> Int
numMines size =
    case size of
        Small ->
            10

        Medium ->
            25

        Large ->
            45



-- view


view : Model -> Html Msg
view model =
    let
        className =
            "grid-" ++ (toString (upperLimit model.boardSize))
    in
        case model.gameStatus of
            Playing ->
                div [ id "playing-area", class className ] (viewCells model)

            Waiting ->
                div []
                    [ button [ onClick (Play Small) ] [ text "Easy" ]
                    , button [ onClick (Play Medium) ] [ text "Medium" ]
                    , button [ onClick (Play Large) ] [ text "Hard" ]
                    ]


viewCells : Model -> List (Html Msg)
viewCells model =
    List.concat (List.map (cellRow model) (gameBoard model.boardSize))


cellRow : Model -> X -> List (Html Msg)
cellRow model row =
    List.map (cell model row) (gameBoard model.boardSize)


cell : Model -> X -> Y -> Html Msg
cell model row col =
    let
        point =
            ( row, col )

        classNames =
            if List.member point model.flags then
                "cell flag"
            else
                "cell"
    in
        div [ class classNames, onClick (Guess point), onRightClick (NewFlag point) ] []


onRightClick : Msg -> Attribute Msg
onRightClick message =
    onWithOptions
        "oncontextmenu"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.succeed message)



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
