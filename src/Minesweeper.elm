module Minesweeper exposing (main)

import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json
import Random
import Random.Set as Random
import Set exposing (Set)


type alias Flags =
    ()


type alias Model =
    { game : Game
    , ctrlPressed : Bool
    }


type alias Game =
    { state : GameState
    , xSize : Int
    , ySize : Int
    , mines : Int
    , mineCoords : Maybe (List Coord)
    , board : Board
    }


type GameState
    = Ready
    | Active
    | Lost
    | Won


type alias Coord =
    ( Int, Int )


type alias Board =
    Dict Coord CellState


type CellState
    = Unclicked { pushed : Bool }
    | Num Int
    | Flagged
    | Mine


type Msg
    = NewGame
    | GeneratedMineCoords (List Coord)
    | ToggleFlagCell Coord
      --| SelectCell Coord
    | SinkCell Coord
    | RaiseCell Coord
    | SetCtrlPressed Bool
    | NoOp


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


makeBoard : Int -> Int -> Board
makeBoard xSize ySize =
    let
        xAxis =
            List.range 0 (xSize - 1)

        yAxis =
            List.range 0 (ySize - 1)

        coords =
            xAxis |> List.concatMap (\x -> yAxis |> List.map (\y -> ( x, y )))
    in
    coords
        |> List.map (\x -> ( x, Unclicked { pushed = False } ))
        |> Dict.fromList


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        xSize =
            8

        ySize =
            8

        mines =
            10

        initGame : Game
        initGame =
            { state = Ready
            , xSize = xSize
            , ySize = ySize
            , mines = mines
            , mineCoords = Nothing
            , board = makeBoard xSize ySize
            }
    in
    ( { game = initGame, ctrlPressed = False }, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Minesweeper"
    , body = [ viewGame model ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        game =
            model.game
    in
    case msg of
        NewGame ->
            ( { model | game = { game | mineCoords = Nothing } }, Cmd.none )

        GeneratedMineCoords coords ->
            ( { model | game = { game | mineCoords = Just coords } }, Cmd.none )

        ToggleFlagCell pos ->
            changeBoard model (atCoord pos toggleFlag)

        --SelectCell pos ->
        --    let
        --        op =
        --            if model.ctrlPressed then
        --                \b -> applyToSurrounding b pos reveal
        --
        --            else
        --                \b -> Dict.get pos b.cells |> Maybe.map (reveal b pos) |> Maybe.withDefault b
        --    in
        --    changeBoard model op
        SinkCell coord ->
            let
                op =
                    if model.ctrlPressed then
                        \b -> applyToSurrounding b coord (\board -> \p -> \_ -> atCoord p (trySinkCell True) board)

                    else
                        atCoord coord (trySinkCell True)
            in
            changeBoard model op

        RaiseCell coord ->
            let
                op =
                    if model.ctrlPressed then
                        \b -> applyToSurrounding b coord (\board -> \p -> \_ -> atCoord p (trySinkCell False) board)

                    else
                        atCoord coord (trySinkCell False)
            in
            changeBoard model op

        SetCtrlPressed ctrlPressed ->
            ( { model | ctrlPressed = ctrlPressed }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onKeyUp (decodeKeyPress False)
        , Events.onKeyDown (decodeKeyPress True)
        ]


viewGame : Model -> Html Msg
viewGame model =
    let
        game =
            model.game

        board =
            game.board

        gameOver =
            case game.state of
                Ready ->
                    Nothing

                Active ->
                    Nothing

                Lost ->
                    Just "Oh no! You hit a mine!"

                Won ->
                    Just "You cleared the field! Well done."
    in
    Html.div [ HtmlA.id "game" ]
        [ viewDetails game
        , viewBoard (gameOver /= Nothing) board
        , gameOver |> Maybe.map viewGameOver |> Maybe.withDefault (Html.text "")
        ]


viewDetails : Game -> Html msg
viewDetails game =
    Html.div [ HtmlA.id "details" ]
        [ Html.p [ HtmlA.class "mines-left", HtmlA.title "Mines Left" ]
            [ game.mines
                - (game.board |> Dict.values |> List.filter (\c -> c == Flagged) |> List.length)
                |> String.fromInt
                |> String.padLeft 3 '0'
                |> Html.text
            ]
        ]


viewGameOver : String -> Html Msg
viewGameOver message =
    Html.div [ HtmlA.id "game-over" ]
        [ Html.div [ HtmlA.class "dialog" ]
            [ Html.p [] [ Html.text message ]
            , Html.button [ HtmlE.onClick NewGame ] [ Html.text "New Game" ]
            ]
        ]


viewBoard : Bool -> Board -> Html Msg
viewBoard gameOver board =
    Html.div [ HtmlA.id "board" ]
        [ Html.div [ HtmlA.class "aspect" ]
            [ board
                |> Dict.toList
                |> List.map (viewCell gameOver)
                |> Html.div [ HtmlA.class "cells" ]
            ]
        ]


viewCell : Bool -> ( Coord, CellState ) -> Html Msg
viewCell gameOver ( coord, state ) =
    let
        ( x, y ) =
            coord

        isNum =
            case state of
                Num _ ->
                    True

                _ ->
                    False

        minesCountClassName =
            case state of
                Num n ->
                    minesCountClass n

                _ ->
                    ""

        interaction =
            if gameOver then
                []

            else
                [ onRightClick (ToggleFlagCell coord)

                --, HtmlE.onClick (SelectCell coord)
                , HtmlE.onMouseDown (SinkCell coord)
                , HtmlE.onMouseLeave (RaiseCell coord)
                ]
    in
    Html.button
        (List.concat
            [ interaction
            , [ HtmlA.classList
                    [ ( "cell", True )
                    , ( "flagged", state == Flagged )
                    , ( "revealed", isNum )

                    --, ( "mined", (isNum || gameOver) && mined )
                    --, ( "wrong", gameOver && not mined && state == Flagged )
                    , ( "pushed", state == Unclicked { pushed = True } )

                    --, ( minesCountClassName, isNum && not mined )
                    ]
              , HtmlA.style "grid-area"
                    (String.fromInt (y + 1) ++ " / " ++ String.fromInt (x + 1) ++ " / auto / auto")
              ]
            ]
        )
        []


minesCountClass : Int -> String
minesCountClass count =
    "m" ++ String.fromInt count


trySinkCell : Bool -> CellState -> CellState
trySinkCell pushed state =
    case state of
        Unclicked _ ->
            Unclicked { pushed = pushed }

        _ ->
            state


decodeKeyPress : Bool -> Json.Decoder Msg
decodeKeyPress changeTo =
    Json.field "key" Json.string
        |> Json.map
            (\k ->
                if k == "Control" then
                    SetCtrlPressed changeTo

                else
                    NoOp
            )


changeBoard : Model -> (Board -> Board) -> ( Model, Cmd msg )
changeBoard model change =
    let
        game =
            model.game

        board =
            game.board

        changed =
            change board
    in
    ( { model | game = { game | state = gameState changed game.mineCoords, board = changed } }
    , Cmd.none
    )


gameState : Board -> Maybe (List Coord) -> GameState
gameState board mineCoords =
    let
        values =
            board |> Dict.values

        items =
            board |> Dict.toList

        isNum cell =
            case cell of
                Num _ ->
                    True

                _ ->
                    False
    in
    case mineCoords of
        Nothing ->
            Ready

        Just mCoords ->
            if values |> List.any (\c -> c == Mine) then
                Lost

            else if
                items
                    |> List.all
                        (\( coord, cell ) -> List.member coord mCoords || isNum cell)
            then
                Won

            else
                Active


minesCount : List Coord -> List Coord -> Int
minesCount mineCoords coords =
    coords
        |> List.map
            (\x ->
                if List.member x mineCoords then
                    1

                else
                    0
            )
        |> List.sum


minesInCell : CellState -> Int
minesInCell state =
    case state of
        Mine ->
            1

        _ ->
            0


surrounding : Coord -> List Coord
surrounding ( x, y ) =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]
        |> List.map (\( x2, y2 ) -> ( x + x2, y + y2 ))


nbrMines : Coord -> List Coord -> Int
nbrMines coord mineCoords =
    coord |> surrounding |> minesCount mineCoords


atCoord : Coord -> (CellState -> CellState) -> Board -> Board
atCoord coord change board =
    board |> Dict.update coord (Maybe.map change)



--reveal : Board -> Coord -> CellState -> Board
--reveal board coord state =
--    let
--        ( newCell, cellChanged ) =
--            trySelectCell coord state
--    in
--    if cellChanged then
--        let
--            changedBoard =
--                Dict.insert coord newCell board
--        in
--        if newCell.mined then
--            changedBoard
--
--        else if nbrMines coord mineCoords then
--            applyToSurrounding changedBoard coord reveal
--
--        else
--            changedBoard
--
--    else
--        board


applyToSurrounding : Board -> Coord -> (Board -> Coord -> CellState -> Board) -> Board
applyToSurrounding board coord f =
    surrounding coord
        |> List.filterMap (\pos -> Dict.get pos board |> Maybe.map (\c -> ( pos, c )))
        |> List.foldl (\( pos, c ) -> \b -> f b pos c) board



--
--trySelectCell : Coord -> CellState -> ( CellState, Bool )
--trySelectCell coord state =
--    let
--        newState =
--            case state of
--                Unclicked _ ->
--                    Num (nbrMines coord mineCoords)
--
--                x ->
--                    x
--    in
--    ( newState, state /= newState )


toggleFlag : CellState -> CellState
toggleFlag state =
    case state of
        Unclicked _ ->
            Flagged

        Flagged ->
            Unclicked { pushed = False }

        x ->
            x


randomMineCoordGenerator : Int -> Int -> Random.Generator (Set Coord)
randomMineCoordGenerator size mines =
    randomCoordGenerator size |> Random.set mines


randomCoordGenerator : Int -> Random.Generator Coord
randomCoordGenerator size =
    Random.map2 (\x -> \y -> ( x, y ))
        (Random.int 0 (size - 1))
        (Random.int 0 (size - 1))


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    { message = msg
    , stopPropagation = True
    , preventDefault = True
    }
        |> Json.succeed
        |> HtmlE.custom "contextmenu"
