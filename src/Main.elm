module Main exposing (Model, Platform, Player, init)

import Browser
import Browser.Dom
import Browser.Events as Browser
import Html exposing (Html)
import Html.Attributes exposing (tabindex)
import Json.Decode as Decode
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Player =
    { x : Float
    , y : Float
    , direction : Direction
    }


type Direction
    = Up
    | Down
    | Right
    | Left


type alias Platform =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Model =
    { player : Player
    , platforms : List Platform
    , screenWidth : Float
    , screenHeight : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { player = { x = 5, y = 5, direction = Down }
      , platforms = [ { x = 0, y = 30, width = 200, height = 10 } ]
      , screenWidth = 0
      , screenHeight = 0
      }
    , Task.perform GotViewPort Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.onKeyDown decodeKey
        , Browser.onResize UpdateScreenWidth
        ]


type Msg
    = MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown
    | NoOp
    | GotViewPort Browser.Dom.Viewport
    | UpdateScreenWidth Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveLeft ->
            let
                movePlayerLeft : Player -> Player
                movePlayerLeft player =
                    { player
                        | x =
                            if model.player.x - 15 == 0 then
                                model.player.x

                            else
                                model.player.x - 5
                    }
            in
            ( { model | player = movePlayerLeft model.player }, Cmd.none )

        MoveRight ->
            let
                movePlayerRight : Player -> Player
                movePlayerRight player =
                    { player
                        | x =
                            if model.player.x + 5 >= model.screenWidth - 55 then
                                model.player.x

                            else
                                model.player.x + 5
                    }
            in
            ( { model | player = movePlayerRight model.player }, Cmd.none )

        MoveUp ->
            let
                movePlayerUp : Player -> Player
                movePlayerUp player =
                    { player
                        | y =
                            if model.player.y - 15 == 0 then
                                model.player.y

                            else
                                model.player.y - 5
                    }
            in
            ( { model | player = movePlayerUp model.player }, Cmd.none )

        MoveDown ->
            let
                movePlayerDown : Player -> Player
                movePlayerDown player =
                    { player
                        | y =
                            if model.player.y + 5 >= model.screenHeight - 55 then
                                model.player.y

                            else
                                model.player.y + 5
                    }
            in
            ( { model | player = movePlayerDown model.player }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        GotViewPort viewPort ->
            ( { model
                | screenWidth = viewPort.viewport.width
                , screenHeight = viewPort.viewport.height
              }
            , Cmd.none
            )

        UpdateScreenWidth width height ->
            ( { model
                | screenWidth = width |> toFloat
                , screenHeight = height |> toFloat
              }
            , Cmd.none
            )


decodeKey : Decode.Decoder Msg
decodeKey =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowLeft" ->
                        Decode.succeed MoveLeft

                    "ArrowRight" ->
                        Decode.succeed MoveRight

                    "ArrowUp" ->
                        Decode.succeed MoveUp

                    "ArrowDown" ->
                        Decode.succeed MoveDown

                    _ ->
                        Decode.succeed NoOp
            )


view : Model -> Html Msg
view model =
    Html.div [ tabindex 0, Html.Attributes.style "overflow" "hidden" ]
        [ viewPlayer model.player ]


viewPlayer : Player -> Html msg
viewPlayer player =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" (String.fromFloat player.x ++ "px")
        , Html.Attributes.style "top" (String.fromFloat player.y ++ "px")
        , Html.Attributes.style "width" "20px"
        , Html.Attributes.style "height" "20px"
        , Html.Attributes.style "background" "blue"
        ]
        []
