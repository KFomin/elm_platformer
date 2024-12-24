module Main exposing (Model, Platform, Player, init)

import Browser
import Browser.Dom
import Browser.Events as Browser
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attrs
import Json.Decode as Decode
import Task
import Time


main : Program Flags Model Msg
main =
    Browser.application
        { init = \_ _ _ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \_ -> NoOp
        }


type alias Flags =
    {}


type alias Player =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , speed : Speed
    }


type Speed
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight


type alias Platform =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type alias Model =
    { player : Player
    , platforms : List Platform
    , screenWidth : Int
    , screenHeight : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { player = { x = 0, y = 0, width = 20, height = 20, speed = One }
      , platforms = [ { x = 0, y = 500, width = 1200, height = 20 } ]
      , screenWidth = 0
      , screenHeight = 0
      }
    , Task.perform GotViewPort Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions { player, platforms } =
    Sub.batch
        [ Browser.onKeyDown (decodeKey player)
        , Browser.onResize UpdateScreenWidth
        , if player.speed == Zero then
            Sub.none

          else
            Time.every 50 (\_ -> MoveDown player)
        ]


type Msg
    = MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown Player
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
                            if model.player.x - 5 == 0 then
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
                            if model.player.x + 5 >= model.screenWidth - 5 then
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
                            if model.player.y - 5 == 0 then
                                model.player.y

                            else
                                model.player.y - 5
                    }
            in
            ( { model | player = movePlayerUp model.player }, Cmd.none )

        MoveDown player ->
            let
                theYCoordinateToStop : Maybe Int
                theYCoordinateToStop =
                    model.platforms
                        |> List.filter
                            (\platform ->
                                ((player.x < (platform.x + platform.width))
                                    && ((player.x + player.width) > platform.x)
                                )
                                    && ((player.y + moveBySpeed player.speed) > (platform.y - platform.height))
                            )
                        |> List.head
                        |> Maybe.map
                            (\platform ->
                                platform.y - platform.height
                            )
            in
            theYCoordinateToStop
                |> Maybe.map
                    (\y ->
                        ( { model | player = { player | y = y, speed = Zero } }, Cmd.none )
                    )
                |> Maybe.withDefault
                    ( { model
                        | player =
                            { player
                                | y = player.y + moveBySpeed player.speed
                                , speed = increaseSpeed player.speed
                            }
                      }
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )

        GotViewPort viewPort ->
            ( { model
                | screenWidth = viewPort.viewport.width |> round
                , screenHeight = viewPort.viewport.height |> round
              }
            , Cmd.none
            )

        UpdateScreenWidth width height ->
            ( { model
                | screenWidth = width
                , screenHeight = height
              }
            , Cmd.none
            )


decodeKey : Player -> Decode.Decoder Msg
decodeKey player =
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
                        Decode.succeed (MoveDown player)

                    _ ->
                        Decode.succeed NoOp
            )


moveBySpeed : Speed -> Int
moveBySpeed speed =
    case speed of
        Zero ->
            0

        One ->
            3

        Two ->
            6

        Three ->
            9

        Four ->
            12

        Five ->
            15

        Six ->
            18

        Seven ->
            21

        Eight ->
            24


increaseSpeed : Speed -> Speed
increaseSpeed speed =
    case speed of
        Zero ->
            One

        One ->
            Two

        Two ->
            Three

        Three ->
            Four

        Four ->
            Five

        Five ->
            Six

        Six ->
            Seven

        Seven ->
            Eight

        Eight ->
            Eight


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Platformer"
    , body =
        [ Html.toUnstyled <|
            Html.div
                [ Attrs.tabindex 0
                , Attrs.css [ Css.overflow Css.hidden ]
                ]
                [ viewAir model
                ]
        ]
    }


viewAir : Model -> Html.Html Msg
viewAir model =
    Html.div [ Attrs.id "platformer-air" ]
        ([ viewPlayer model.player
         ]
            ++ (model.platforms
                    |> List.map viewPlatform
               )
        )


viewPlayer : Player -> Html.Html Msg
viewPlayer player =
    Html.div
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.left (Css.px (toFloat player.x))
            , Css.top (Css.px (toFloat player.y))
            , Css.width (Css.px (toFloat player.width))
            , Css.height (Css.px (toFloat player.height))
            , Css.backgroundColor (Css.hex "#0000FF")
            ]
        , Attrs.id "platformer-player"
        ]
        []


viewPlatform : Platform -> Html.Html Msg
viewPlatform platform =
    Html.div
        [ Attrs.css
            [ Css.width (Css.px (toFloat platform.width))
            , Css.height (Css.px (toFloat platform.height))
            , Css.top (Css.px (toFloat platform.y))
            , Css.left (Css.px (toFloat platform.x))
            , Css.backgroundColor (Css.hex "#000000")
            , Css.position Css.absolute
            , Css.overflow Css.hidden
            ]
        , Attrs.id "platformer-platform"
        ]
        []
