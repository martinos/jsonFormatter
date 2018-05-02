module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import JsonTree


type alias Model =
    { input : String, output : Result String String, state : JsonTree.State, display : DisplayType }


type DisplayType
    = TreeView
    | TextView


model : Model
model =
    { input = "{}", output = Ok "{}", state = JsonTree.defaultState, display = TextView }


main =
    Html.program { init = ( model, Cmd.none ), update = update, view = view, subscriptions = always Sub.none }


type Msg
    = SetInput String
    | SetState JsonTree.State
    | SetDisplay DisplayType


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

        SetState state ->
            { model | state = state } ! [ Cmd.none ]

        SetDisplay display ->
            { model | display = display } ! [ Cmd.none ]


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
                    , div [ class "column is-half" ]
                        [ div [ class "tabs is-centered" ]
                            [ ul []
                                [ li [ classList [ ( "is-active", model.display == TextView ) ] ]
                                    [ a [ onClick <| SetDisplay TextView ] [ text "Text" ] ]
                                , li [ classList [ ( "is-active", model.display == TreeView ) ] ]
                                    [ a [ onClick <| SetDisplay TreeView ] [ text "Tree" ] ]
                                ]
                            ]
                        , viewOutput model
                        ]
                    ]
                ]
            ]
        ]


viewOutput model =
    case model.display of
        TreeView ->
            treeView model.state model.input

        TextView ->
            textView model.output


treeView state input =
    case input |> JsonTree.parseString of
        Ok tree ->
            pre [] [ JsonTree.view tree config state ]

        Err err ->
            div []
                [ div [ class "notification is-danger" ] [ p [] [ err |> text ] ] ]


toggleDisplay display =
    case display of
        TextView ->
            TreeView

        TreeView ->
            TextView


textView output =
    case output of
        Ok str ->
            pre [ id "copy-me", style [ ( "position", "relative" ), ( "overflow", "scroll" ) ] ]
                [ str |> text
                , copyButton
                ]

        Err err ->
            div []
                [ div [ class "notification is-danger" ] [ p [] [ err |> text ] ]
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


config =
    { onSelect = Nothing, toMsg = SetState }
