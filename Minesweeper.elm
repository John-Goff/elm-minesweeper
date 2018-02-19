module Minesweeper exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Html.Attributes exposing (class, id)
import Random exposing (Generator, int, pair)
import Json.Decode as Json
import Task exposing (Task)


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
    , revealed : List ValPoint
    , boardSize : Size
    , gameStatus : GameState
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


type alias Point =
    ( X, Y )


type alias ValPoint =
    { value : Int
    , point : Point
    }


initialModel : Model
initialModel =
    { mines = []
    , flags = []
    , revealed = []
    , boardSize = Small
    , gameStatus = Waiting ""
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
            guess p model

        Play s ->
            ( { initialModel | gameStatus = Playing, boardSize = s }, Random.generate NewMine (newPoint s) )

        GameOver ->
            ( { model | gameStatus = Waiting "Game Over!" }, Cmd.none )


guess : Point -> Model -> ( Model, Cmd Msg )
guess p model =
    let
        pointValue =
            calcValue p model.mines

        newModel =
            { model | revealed = ({ point = p, value = pointValue } :: model.revealed) }
    in
        if List.member p model.mines then
            update GameOver model
        else if pointValue == 0 then
            ( newModel, Cmd.batch (guessEach (surrounds p)) )
        else
            ( newModel, Cmd.none )


guessEach : List Point -> List (Cmd Msg)
guessEach surrounding =
    List.map (Task.perform Guess) (List.map Task.succeed surrounding)


numMines : Size -> Int
numMines size =
    case size of
        Small ->
            10

        Medium ->
            25

        Large ->
            45


calcValue : Point -> List Point -> Int
calcValue point mines =
    let
        surrPoints =
            surrounds point
    in
        List.length (List.filter (\x -> List.member x mines) surrPoints)



{--
x-1y+1 xy+1 x+1y+1
  x-1y xy   x+1y
x-1y-1 xy-1 x+1y-1
--}


surrounds : Point -> List Point
surrounds point =
    let
        ( x, y ) =
            point
    in
        [ ( x - 1, y + 1 )
        , ( x, y + 1 )
        , ( x + 1, y + 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        , ( x - 1, y - 1 )
        , ( x, y - 1 )
        , ( x + 1, y - 1 )
        ]



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

            Waiting t ->
                div []
                    [ text t
                    , button [ onClick (Play Small) ] [ text "Easy" ]
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

        zeros =
            pointFromValPoint (pointWeight 0) model.revealed

        ones =
            pointFromValPoint (pointWeight 1) model.revealed

        twos =
            pointFromValPoint (pointWeight 2) model.revealed

        threes =
            pointFromValPoint (pointWeight 3) model.revealed

        fours =
            pointFromValPoint (pointWeight 4) model.revealed

        fives =
            pointFromValPoint (pointWeight 5) model.revealed

        sixes =
            pointFromValPoint (pointWeight 6) model.revealed

        sevens =
            pointFromValPoint (pointWeight 7) model.revealed

        eights =
            pointFromValPoint (pointWeight 8) model.revealed

        classNames =
            if List.member point model.flags then
                "cell flag"
            else if List.member point ones then
                "cell one"
            else if List.member point twos then
                "cell two"
            else if List.member point threes then
                "cell three"
            else if List.member point fours then
                "cell four"
            else if List.member point fives then
                "cell five"
            else if List.member point sixes then
                "cell six"
            else if List.member point sevens then
                "cell seven"
            else if List.member point eights then
                "cell eight"
            else if List.member point zeros then
                "cell revealed"
            else
                "cell"
    in
        div [ class classNames, onClick (Guess point), onRightClick (NewFlag point) ] []


onRightClick : Msg -> Attribute Msg
onRightClick message =
    onWithOptions "contextmenu" { stopPropagation = True, preventDefault = True } (Json.succeed message)


pointFromValPoint : (ValPoint -> List a -> List a) -> List ValPoint -> List a
pointFromValPoint func =
    List.foldl func []


pointWeight : Int -> ValPoint -> List Point -> List Point
pointWeight num x a =
    if x.value == num then
        x.point :: a
    else
        a



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
