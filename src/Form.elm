module Form exposing (..)

import Browser.Dom
import Common exposing (Point, ReferenceFrame)
import Html exposing (Html, button, div, h5, input, p, text)
import Html.Attributes exposing (disabled, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Task


type Form
    = NoForm
    | RefForm RefFormData


type alias RefFormData =
    { inputDistance : String
    , inputUnit : String
    , p1 : Point
    , p2 : Point
    }



-- UPDATE


type Msg
    = ApplyForm
    | CancelForm
    | FocusResult (Result Browser.Dom.Error ())
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

        ApplyForm ->
            -- This must be handled upstream
            ( model, Cmd.none, False )

        CancelForm ->
            -- This must be handled upstream
            ( model, Cmd.none, False )

        FocusResult _ ->
            -- Don't really care about the result
            ( model, Cmd.none, False )


view : (Msg -> msg) -> Form -> Html msg
view toMsg model =
    case model of
        NoForm ->
            text ""

        RefForm refForm ->
            viewRef toMsg refForm


viewRef : (Msg -> msg) -> RefFormData -> Html msg
viewRef toMsg refForm =
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
                [ onClick <| toMsg ApplyForm
                , disabled <| distance == Nothing
                ]
                [ text "Apply" ]
            , button
                [ --onFocus StopCapture
                  --, onBlur StartCapture
                  onClick <| toMsg CancelForm
                ]
                [ text "Clear" ]
            ]
        ]
