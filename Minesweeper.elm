module Minesweeper exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id)
import Random exposing (Generator, int, list, map2)
import Random.List exposing (shuffle)


-- msg


type Msg
    = BeginGame
    | NewBoard Board


type Cell
    = Open CellData
    | Closed CellData


type alias CellData =
    { status : CellStatus, point : Point, adjacent : Adjacent }


type CellStatus
    = Flag
    | Mine
    | Clear



-- generators


generateBoard : Size -> Generator Board
generateBoard size =
    let
        boardSizeInt =
            size
                |> upperLimit
                |> indexByZero

        numMines =
            (upperLimit size) // 5

        listOfClear =
            List.repeat ((upperLimit size) - numMines)
                (Closed { status = Clear, point = InvalidPoint, adjacent = 0 })

        listOfMines =
            List.repeat numMines (Closed { status = Mine, point = InvalidPoint, adjacent = 0 })
    in
        listOfClear
            ++ listOfMines
            |> shuffle


newPoint : Generator Point
newPoint =
    let
        intGen =
            int 0 (upperLimit Small)
    in
        map2 Point intGen intGen



-- model


type alias Model =
    { board : Board
    , gameState : GameState
    , boardSize : Size
    }


type Size
    = Small
    | Medium
    | Large


type GameState
    = Playing
    | Waiting String


type alias X =
    Int


type alias Y =
    Int


type alias Adjacent =
    Int


type alias Board =
    List Cell


type Point
    = Point X Y
    | InvalidPoint


initialModel : Model
initialModel =
    { board = []
    , gameState = Waiting "Please click to begin"
    , boardSize = Small
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
    size
        |> upperLimit
        |> indexByZero
        |> List.range 0


indexByZero : Int -> Int
indexByZero num =
    num - 1


init : ( Model, Cmd Msg )
init =
    ( initialModel, Random.generate NewBoard (generateBoard initialModel.boardSize) )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BeginGame ->
            ( { model | gameState = Playing }, Cmd.none )

        NewBoard b ->
            ( { model | board = (mapNewBoard model.boardSize b) }, Cmd.none )


mapNewBoard : Size -> Board -> Board
mapNewBoard size board =
    let
        boardWithPoints =
            List.concatMap (generateBoardRow size) (gameBoard size)
    in
        List.map2 mapPoints board boardWithPoints


mapPoints : Cell -> Cell -> Cell
mapPoints oldCell cell =
    let
        cellData =
            case cell of
                Open cd ->
                    cd

                Closed cd ->
                    cd
    in
        case oldCell of
            Open cd ->
                Open { cd | point = cellData.point }

            Closed cd ->
                Closed { cd | point = cellData.point }


generateBoardRow : Size -> X -> Board
generateBoardRow size x =
    List.map (mapCell x) (gameBoard size)


mapCell : X -> Y -> Cell
mapCell x y =
    Closed { status = Clear, point = (Point x y), adjacent = 0 }


view : Model -> Html Msg
view model =
    case model.gameState of
        Waiting m ->
            div []
                [ button [] [ text "Click to begin" ]
                , waitingGameView m
                ]

        Playing ->
            div [ id "playing-area" ]
                [ button [] [ text "Small" ]
                , button [] [ text "Medium" ]
                , button [] [ text "Large" ]
                , playingGameView
                ]


waitingGameView : String -> Html Msg
waitingGameView waitingMessage =
    div [ id "waiting" ]
        [ h1 [] [ text waitingMessage ] ]


playingGameView : Html Msg
playingGameView =
    div [ id "playing-area" ] []



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
