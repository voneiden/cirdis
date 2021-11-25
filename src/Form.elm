module Form exposing (..)

import Browser.Dom
import Common exposing (Point, ReferenceFrame)
import Html exposing (Html, button, div, h3, h4, h5, input, p, text)
import Html.Attributes exposing (class, disabled, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Task


type Form
    = WelcomeForm
    | NoForm
    | RefForm RefFormData


type alias RefFormData =
    { inputDistance : String
    , inputUnit : String
    , p1 : Point
    , p2 : Point
    }


type alias ViewData msg =
    { welcome : ViewWelcome msg
    , ref : ViewRef msg
    }


type alias ViewWelcome msg =
    { importLayerMsg : msg
    , importSvgMsg : msg
    }


type alias ViewRef msg =
    { apply : RefFormData -> msg
    , clear : msg
    }



-- UPDATE


type Msg
    = FocusResult (Result Browser.Dom.Error ())
    | RefMsg RefMsg


type RefMsg
    = RefInit (Maybe ReferenceFrame) Point Point
    | RefInputDistance String
    | RefInputUnit String


type alias ModelForm a =
    { a | form : Form }


update : (Msg -> msg) -> Msg -> ModelForm a -> ( ModelForm a, Cmd msg, Bool )
update toMsg msg model =
    case msg of
        RefMsg refMsg ->
            case ( model.form, refMsg ) of
                ( _, RefInit maybeReferenceFrame p1 p2 ) ->
                    let
                        cmd =
                            Browser.Dom.focus "reference-distance-input" |> Task.attempt (toMsg << FocusResult)
                    in
                    case maybeReferenceFrame of
                        Just referenceFrame ->
                            ( { model
                                | form =
                                    RefForm
                                        { inputDistance = String.fromFloat referenceFrame.value
                                        , inputUnit = referenceFrame.unit
                                        , p1 = p1
                                        , p2 = p2
                                        }
                              }
                            , cmd
                            , False
                            )

                        Nothing ->
                            ( { model
                                | form =
                                    RefForm
                                        { inputDistance = ""
                                        , inputUnit = ""
                                        , p1 = p1
                                        , p2 = p2
                                        }
                              }
                            , cmd
                            , False
                            )

                ( RefForm data, RefInputDistance inputDistance ) ->
                    ( { model | form = RefForm { data | inputDistance = inputDistance } }, Cmd.none, False )

                ( RefForm data, RefInputUnit inputUnit ) ->
                    ( { model | form = RefForm { data | inputUnit = inputUnit } }, Cmd.none, False )

                _ ->
                    ( model, Cmd.none, False )

        FocusResult _ ->
            -- Don't really care about the result
            ( model, Cmd.none, False )


view : ViewData msg -> (Msg -> msg) -> Form -> Html msg
view viewData toMsg model =
    case model of
        WelcomeForm ->
            viewWelcome viewData.welcome

        NoForm ->
            text ""

        RefForm refForm ->
            viewRef viewData.ref toMsg refForm


viewWelcome : ViewWelcome msg -> Html msg
viewWelcome welcome =
    div []
        [ h3 [] [ text "Welcome to Circuit Dissector" ]
        , p [] [ text "Please see the docs at TODO" ]
        , p []
            [ div [] [ text "Get started with" ]
            , div [ class "import-layer" ]
                [ button [ onClick <| welcome.importLayerMsg ] [ text <| "Import layer" ]
                , button [ onClick <| welcome.importSvgMsg ] [ text <| "Import project" ]
                ]
            ]
        ]


viewRef : ViewRef msg -> (Msg -> msg) -> RefFormData -> Html msg
viewRef ref toMsg refForm =
    let
        distance =
            String.toFloat refForm.inputDistance
    in
    div []
        [ h5 [] [ text "Provide reference details" ]
        , p []
            [ div [] [ text "Distance" ]
            , input
                [ id "reference-distance-input"

                --, onFocus FocusDistance
                --, onBlur BlurDistance
                , value refForm.inputDistance -- TODO default needs to come somehow from ref?
                , onInput <| toMsg << RefMsg << RefInputDistance
                ]
                []
            ]
        , p []
            [ div [] [ text "Value" ]
            , input
                [ -- onFocus FocusUnit
                  --, onBlur BlurUnit
                  placeholder "mm"
                , value refForm.inputUnit
                , onInput <| toMsg << RefMsg << RefInputUnit
                ]
                []
            ]
        , p []
            [ button
                [ onClick <| ref.apply refForm
                , disabled <| distance == Nothing
                ]
                [ text "Apply" ]
            , button
                [ --onFocus StopCapture
                  --, onBlur StartCapture
                  onClick ref.clear
                ]
                [ text "Remove" ]
            ]
        ]
