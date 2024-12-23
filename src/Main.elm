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
    { x : Float
    , y : Float
    , width : Float
    , height : Float
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


init : ( Model, Cmd Msg )
init =
    ( { player = { x = 5, y = 5, width = 20, height = 20, direction = Down }
      , platforms = [ { x = 0, y = 500, width = 1000, height = 25 } ]
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
        , Time.every 100 (\_ -> MoveDown)
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

        MoveDown ->
            let
                movePlayerDown : Player -> Player
                movePlayerDown player =
                    { player
                        | y =
                            if model.player.y + 5 >= model.screenHeight - 5 then
                                model.player.y

                            else
                                model.player.y + 5
                    }

                cantMoveDown =
                    model.platforms
                        |> List.any
                            (\platform ->
                                ((model.player.x < (platform.x + platform.width))
                                    && ((model.player.x + model.player.width) > platform.x)
                                )
                                    && ((model.player.y < (platform.y + platform.height))
                                            && ((model.player.y + model.player.height) > platform.y)
                                       )
                            )
            in
            if cantMoveDown then
                ( model, Cmd.none )

            else
                ( { model | player = movePlayerDown model.player }
                , Cmd.none
                )

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
            , Css.left (Css.px player.x)
            , Css.top (Css.px player.y)
            , Css.width (Css.px player.width)
            , Css.height (Css.px player.height)
            , Css.backgroundColor (Css.hex "#0000FF")
            ]
        , Attrs.id "platformer-player"
        ]
        []


viewPlatform : Platform -> Html.Html Msg
viewPlatform platform =
    Html.div
        [ Attrs.css
            [ Css.width (Css.px platform.width)
            , Css.height (Css.px platform.height)
            , Css.top (Css.px platform.y)
            , Css.left (Css.px platform.x)
            , Css.backgroundColor (Css.hex "#000000")
            , Css.position Css.absolute
            ]
        , Attrs.id "platformer-platform"
        ]
        []
