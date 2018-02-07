module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE


type alias Model =
    { input : String, output : Result String String }


model : Model
model =
    { input = "{}", output = Ok "{}" }


main =
    Html.program { init = ( model, Cmd.none ), update = update, view = view, subscriptions = always Sub.none }


type Msg
    = SetInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetInput str ->
            ( { model
                | input = str
                , output =
                    str
                        |> JD.decodeString JD.value
                        |> Result.map (JE.encode 2)
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ stylesheet "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.6.1/css/bulma.min.css"
        , section [ class "section" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ]
                    [ text "JSON pretty" ]
                , div [ class "columns" ]
                    [ div [ class "column is-half" ]
                        [ p [ class "subtitle" ] [ text "Input" ]
                        , Html.form []
                            [ textarea
                                [ class "textarea"
                                , attribute "rows" "30"
                                , placeholder "Enter your json here"
                                , value model.input
                                , onInput SetInput
                                ]
                                []
                            ]
                        ]
                    , div [ class "column is-half" ] [ viewOutput model.output ]
                    ]
                ]
            ]
        ]


viewOutput output =
    case output of
        Ok str ->
            div []
                [ p [ class "subtitle" ]
                    [ text "Output" ]
                , pre [ id "copy-me", style [ ( "position", "relative" ), ( "overflow", "scroll" ) ] ]
                    [ str |> text
                    , copyButton
                    ]
                ]

        Err err ->
            div []
                [ p [ class "subtitle" ] [ text "Output" ]
                , div [ class "notification is-danger" ] [ p [] [ err |> text ] ]
                ]


copyButton =
    button
        [ class "copy-button button is-small"
        , attribute "data-clipboard-target" "#copy-me"
        , style [ ( "position", "absolute" ), ( "top", "0.25rem" ), ( "right", "0.25rem" ) ]
        ]
        [ "Copy" |> text ]


stylesheet : String -> Html msg
stylesheet url =
    node "link"
        [ rel "stylesheet"
        , href url
        ]
        []
