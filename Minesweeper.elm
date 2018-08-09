module Minesweeper exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Random exposing (Generator, int, list, map2)
import Random.List exposing (shuffle)


-- msg


type Msg
    = BeginGame
    | NewBoard Board
    | RevealCell


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
            List.repeat (((upperLimit size) ^ 2) - numMines)
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
    , boardSize = Medium
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
            ( { model | board = (updateBoardWithPoints model.boardSize b) }, Cmd.none )

        RevealCell ->
            ( model, Cmd.none )


updateBoardWithPoints : Size -> Board -> Board
updateBoardWithPoints size board =
    let
        boardWithPoints =
            List.concatMap (generateBoardRow size) (gameBoard size)
    in
        List.map2 mapPoints (Debug.log "board" board) boardWithPoints


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
                [ button [ onClick BeginGame ] [ text "Click to begin" ]
                , waitingGameView m
                ]

        Playing ->
            div [ id "playing-area" ]
                [ button [] [ text "Small" ]
                , button [] [ text "Medium" ]
                , button [] [ text "Large" ]
                , playingGameView model
                ]


waitingGameView : String -> Html Msg
waitingGameView waitingMessage =
    div [ id "waiting" ]
        [ h1 [] [ text waitingMessage ] ]


playingGameView : Model -> Html Msg
playingGameView model =
    let
        className =
            case model.boardSize of
                Small ->
                    "grid-15"

                Medium ->
                    "grid-30"

                Large ->
                    "grid-45"
    in
        div [ id "playing-area", class className ] (listBoard model.board)


listBoard : Board -> List (Html Msg)
listBoard board =
    List.map cellView board


cellView : Cell -> Html Msg
cellView cell =
    case cell of
        Closed cd ->
            div [ class "cell", onClick RevealCell ] []

        Open cd ->
            div [ class "cell revealed" ] []


pointFromCell : Cell -> Point
pointFromCell cell =
    case cell of
        Open cd ->
            cd.point

        Closed cd ->
            cd.point


textFromPoint : Point -> Html Msg
textFromPoint point =
    case point of
        InvalidPoint ->
            text "invalid"

        Point x y ->
            text ((toString x) ++ ", " ++ (toString y))



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
