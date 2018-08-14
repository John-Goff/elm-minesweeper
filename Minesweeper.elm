module Minesweeper exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick, onWithOptions, Options)
import Random exposing (Generator, int, list, map2)
import Random.List exposing (shuffle)
import List.Extra
import Json.Decode as Json


-- msg


type Msg
    = BeginGame Size
    | NewBoard Board
    | RevealCell Cell
    | ChangeCellStatus Cell CellStatus
    | UpdateState GameState


type alias Cell =
    { status : CellStatus
    , cellType : CellType
    , point : Point
    , adjacent : Adjacent
    }


type CellStatus
    = Open
    | Closed
    | Flag
    | FlaggedMine


type CellType
    = Mine
    | Clear


type Point
    = Point ( X, Y )
    | InvalidPoint


type alias X =
    Int


type alias Y =
    Int


type alias Adjacent =
    Int



-- generators


generateBoard : Size -> Generator Board
generateBoard size =
    let
        numMines =
            case size of
                Small ->
                    15

                Medium ->
                    90

                Large ->
                    150

        listOfClear =
            List.repeat (((upperLimit size) ^ 2) - numMines) (Cell Closed Clear InvalidPoint 0)

        listOfMines =
            List.repeat numMines (Cell Closed Mine InvalidPoint 0)
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


type alias Board =
    List Cell


type GameState
    = Playing
    | Waiting
    | GameOver
    | Victory


type Size
    = Small
    | Medium
    | Large


initialModel : Model
initialModel =
    { board = []
    , gameState = Waiting
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
        |> (\num -> num - 1)
        |> List.range 0


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BeginGame size ->
            ( { model | gameState = Playing, boardSize = size }
            , Random.generate NewBoard (generateBoard size)
            )

        NewBoard b ->
            ( { model | board = newBoard model.boardSize b }, Cmd.none )

        RevealCell c ->
            let
                updatedBoard =
                    if c.adjacent == 0 then
                        revealAllCells [ c ] model.board
                    else
                        markCellOpen c model.board

                updatedModel =
                    { model | board = updatedBoard }

                cellsWithoutMines =
                    List.filter (\c -> c.cellType == Clear) updatedBoard
            in
                if List.all (\c -> c.status == Open) cellsWithoutMines then
                    update (UpdateState Victory) updatedModel
                else
                    case c.cellType of
                        Clear ->
                            ( updatedModel, Cmd.none )

                        Mine ->
                            update (UpdateState GameOver) { model | board = revealMines updatedBoard }

        ChangeCellStatus cell status ->
            ( { model | board = List.Extra.replaceIf ((==) cell) { cell | status = status } model.board }, Cmd.none )

        UpdateState gs ->
            ( { model | gameState = gs }, Cmd.none )


revealMines : Board -> Board
revealMines =
    List.map revealMine


revealMine : Cell -> Cell
revealMine cell =
    if cell.cellType == Mine then
        if cell.status == Flag then
            { cell | status = FlaggedMine }
        else
            { cell | status = Open }
    else
        cell


revealAllCells : List Cell -> Board -> Board
revealAllCells cellsToProcess board =
    case cellsToProcess of
        [] ->
            board

        head :: remainder ->
            let
                newBoard =
                    markCellOpen head board

                cellsToAdd =
                    List.filterMap (isCellInRemainder remainder) (adjacentInBoardToCell board head)
            in
                if head.adjacent == 0 then
                    revealAllCells (remainder ++ cellsToAdd) newBoard
                else
                    revealAllCells remainder newBoard


markCellOpen : Cell -> Board -> Board
markCellOpen cell board =
    List.Extra.replaceIf ((==) cell) { cell | status = Open } board


isCellInRemainder : List Cell -> Cell -> Maybe Cell
isCellInRemainder remainder cell =
    if List.member cell remainder then
        Nothing
    else if cell.status == Open then
        Nothing
    else
        Just cell


newBoard : Size -> Board -> Board
newBoard size board =
    board
        |> updateBoardWithPoints size
        |> updateAdjacent


updateBoardWithPoints : Size -> Board -> Board
updateBoardWithPoints size board =
    List.map2 mapPoints board (boardWithPoints size)


boardWithPoints : Size -> Board
boardWithPoints size =
    List.concatMap (generateBoardRow size) (gameBoard size)


updateAdjacent : Board -> Board
updateAdjacent board =
    List.map (calculateAdjacent board) board


calculateAdjacent : Board -> Cell -> Cell
calculateAdjacent board cell =
    let
        numAdjacent =
            cell
                |> adjacentInBoardToCell board
                |> List.foldr sumMines 0
    in
        { cell | adjacent = numAdjacent }


sumMines : Cell -> Int -> Int
sumMines cell total =
    if cell.cellType == Mine then
        total + 1
    else
        total


adjacentInBoardToCell : Board -> Cell -> List Cell
adjacentInBoardToCell board cell =
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

        [ a ] ->
            Just a

        _ ->
            Nothing


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
        Waiting ->
            waitingView model.boardSize "Click to begin" "Please click to begin"

        Playing ->
            div [ id "playing-area" ]
                [ button [ onClick (BeginGame Small) ] [ text "New Game (Small)" ]
                , button [ onClick (BeginGame Medium) ] [ text "New Game (Medium)" ]
                , button [ onClick (BeginGame Large) ] [ text "New Game (Large)" ]
                , playingGameView model
                ]

        GameOver ->
            finishedView model "Play Again?" "Game Over!"

        Victory ->
            finishedView model "Play Again?" "You Won!"


onRightClick : Msg -> Attribute Msg
onRightClick message =
    onWithOptions "contextmenu" (Options True True) (Json.succeed message)


waitingView : Size -> String -> String -> Html Msg
waitingView size buttonText headerText =
    div []
        [ button [ onClick (BeginGame size) ] [ text buttonText ]
        , div [ id "waiting" ] [ h1 [] [ text headerText ] ]
        ]


finishedView : Model -> String -> String -> Html Msg
finishedView model buttonText headerText =
    div []
        [ button [ onClick (BeginGame model.boardSize) ] [ text buttonText ]
        , div [ id "waiting" ] [ h1 [] [ text headerText ] ]
        , playingGameView model
        ]


playingGameView : Model -> Html Msg
playingGameView model =
    model.gameState
        == Playing
        |> listBoard model.board
        |> div [ id "playing-area", class (gridClassName model.boardSize) ]


gridClassName : Size -> String
gridClassName size =
    case size of
        Small ->
            "grid-15"

        Medium ->
            "grid-30"

        Large ->
            "grid-45"


listBoard : Board -> Bool -> List (Html Msg)
listBoard board clickable =
    List.map (cellView clickable) board


cellView : Bool -> Cell -> Html Msg
cellView clickable cell =
    case cell.status of
        Closed ->
            if clickable then
                div [ class "cell", onClick (RevealCell cell), onRightClick (ChangeCellStatus cell Flag) ] []
            else
                div [ class "cell" ] []

        Open ->
            let
                className =
                    case cell.cellType of
                        Mine ->
                            "mine"

                        Clear ->
                            intToClass cell.adjacent
            in
                div [ class ("cell " ++ className) ] []

        Flag ->
            div [ class "cell flag", onRightClick (ChangeCellStatus cell Closed) ] []

        FlaggedMine ->
            div [ class "cell flagged" ] []


intToClass : Int -> String
intToClass num =
    case num of
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
