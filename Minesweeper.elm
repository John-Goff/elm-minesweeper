module Minesweeper exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Random exposing (Generator, int, list, map2)
import Random.List exposing (shuffle)
import List.Extra


-- msg


type Msg
    = BeginGame
    | NewBoard Board
    | RevealCell Cell Int


type CellStatus
    = Open
    | Closed


type alias Cell =
    { status : CellStatus
    , cellType : CellType
    , point : Point
    , adjacent : Adjacent
    }


type CellType
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
                { status = Closed, cellType = Clear, point = InvalidPoint, adjacent = 0 }

        listOfMines =
            List.repeat numMines { status = Closed, cellType = Mine, point = InvalidPoint, adjacent = 0 }
    in
        listOfClear
            ++ listOfMines
            |> shuffle



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
    = Point ( X, Y )
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
            let
                newBoard =
                    b
                        |> updateBoardWithPoints model.boardSize
                        |> updateAdjacent
            in
                ( { model | board = updateBoardWithPoints model.boardSize b }, Cmd.none )

        RevealCell c i ->
            ( { model | board = List.Extra.setAt i { c | status = Open } model.board }, Cmd.none )


updateBoardWithPoints : Size -> Board -> Board
updateBoardWithPoints size board =
    let
        boardWithPoints =
            List.concatMap (generateBoardRow size) (gameBoard size)
    in
        List.map2 mapPoints board boardWithPoints


updateAdjacent : Board -> Board
updateAdjacent board =
    List.map (calculateAdjacent board) board


calculateAdjacent : Board -> Cell -> Cell
calculateAdjacent board cell =
    let
        numAdjacent =
            cell
                |> adjacentCells board
                |> List.foldr sumMines 0
    in
        { cell | adjacent = numAdjacent }


sumMines : Cell -> Int -> Int
sumMines cell total =
    if cell.cellType == Mine then
        total + 1
    else
        total


adjacentCells : Board -> Cell -> List Cell
adjacentCells board cell =
    let
        adjacentPoints =
            case cell.point of
                Point ( x, y ) ->
                    [ Point ( x - 1, y - 1 ), Point ( x - 1, y ), Point ( x - 1, y + 1 ), Point ( x, y - 1 ), Point ( x, y + 1 ), Point ( x + 1, y - 1 ), Point ( x + 1, y ), Point ( x + 1, y + 1 ) ]

                InvalidPoint ->
                    []
    in
        List.filterMap (cellFromPoint board) adjacentPoints


cellFromPoint : Board -> Point -> Maybe Cell
cellFromPoint board point =
    case List.filter (cellHasPoint point) board of
        [] ->
            Nothing

        a :: _ ->
            Just a


cellHasPoint : Point -> Cell -> Bool
cellHasPoint point cell =
    cell.point == point


mapPoints : Cell -> Cell -> Cell
mapPoints oldCell cell =
    { oldCell | point = cell.point }


generateBoardRow : Size -> X -> List Cell
generateBoardRow size x =
    List.map (mapCell x) (gameBoard size)


mapCell : X -> Y -> Cell
mapCell x y =
    { status = Closed, cellType = Clear, point = (Point ( x, y )), adjacent = 0 }



-- view


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
    List.indexedMap cellView board


cellView : Int -> Cell -> Html Msg
cellView index cell =
    case cell.status of
        Closed ->
            div [ class "cell", onClick (RevealCell cell index) ] []

        Open ->
            let
                className =
                    case cell.cellType of
                        Flag ->
                            "flag"

                        Mine ->
                            "mine"

                        Clear ->
                            case cell.adjacent of
                                1 ->
                                    "one"

                                2 ->
                                    "two"

                                3 ->
                                    "three"

                                4 ->
                                    "four"

                                5 ->
                                    "five"

                                6 ->
                                    "six"

                                7 ->
                                    "seven"

                                8 ->
                                    "eight"

                                _ ->
                                    "revealed"
            in
                div [ class ("cell " ++ className) ] []


textFromPoint : Point -> Html Msg
textFromPoint point =
    case point of
        InvalidPoint ->
            text "invalid"

        Point ( x, y ) ->
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
