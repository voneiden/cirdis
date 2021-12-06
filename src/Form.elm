module Form exposing (..)

import Browser.Dom
import Common exposing (Point, ReferenceFrame)
import Conductor exposing (Net)
import Html exposing (Attribute, Html, button, div, h3, input, p, text)
import Html.Attributes exposing (autocomplete, class, disabled, id, placeholder, value)
import Html.Events exposing (keyCode, onClick, onFocus, onInput, preventDefaultOn)
import Json.Decode as Json exposing (Decoder)
import Task


type Form
    = WelcomeForm
    | NoForm
    | RefForm RefFormData
    | NetForm NetFormData


type alias RefFormData =
    { inputDistance : String
    , inputUnit : String
    , p1 : Point
    , p2 : Point
    }


type alias NetFormData =
    { select : Maybe Net
    , inputName : String
    , inputColor : String
    }


type alias ViewData msg =
    { welcome : ViewWelcome msg
    , ref : ViewRef msg
    , net : ViewNet msg
    }


type alias ViewWelcome msg =
    { importLayerMsg : msg
    , importSvgMsg : msg
    }


type alias ViewRef msg =
    { apply : RefFormData -> msg
    , clear : msg
    }


type alias ViewNet msg =
    { apply : NetFormData -> msg
    , options : List Net
    }



-- UPDATE


type Msg
    = FocusResult (Result Browser.Dom.Error ())
    | Focus String
    | RefMsg RefMsg
    | Nop


type RefMsg
    = RefInit (Maybe ReferenceFrame) Point Point
    | RefInputDistance String
    | RefInputUnit String


enterApply : (Msg -> msg) -> msg -> Attribute msg
enterApply toMsg applyMsg =
    preventDefaultOn "keydown" (Json.map (parseEnter applyMsg (toMsg Nop)) keyCode)


enterFocus : (Msg -> msg) -> String -> Attribute msg
enterFocus toMsg focusId =
    preventDefaultOn "keydown" (Json.map (parseEnter (toMsg (Focus focusId)) (toMsg Nop)) keyCode)


parseEnter : msg -> msg -> Int -> ( msg, Bool )
parseEnter enterMsg elseMsg key =
    if key == 13 then
        ( enterMsg, True )

    else
        ( elseMsg, False )


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

        Focus focusId ->
            let
                cmd =
                    Browser.Dom.focus focusId |> Task.attempt (toMsg << FocusResult)
            in
            ( model, cmd, False )

        Nop ->
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

        NetForm netFormData ->
            viewNet viewData.net toMsg netFormData


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
        [ h3 [] [ text "Provide reference details" ]
        , p []
            [ div [] [ text "Distance" ]
            , input
                [ id "reference-distance-input"
                , autocomplete False
                , enterFocus toMsg "reference-value-input"

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
                [ id "reference-value-input"
                , enterApply toMsg (ref.apply refForm)

                --, onBlur BlurUnit
                , placeholder "mm"
                , autocomplete False
                , value refForm.inputUnit
                , onInput <| toMsg << RefMsg << RefInputUnit
                ]
                []
            ]
        , p []
            [ button
                [ id "reference-apply"
                , onClick <| ref.apply refForm
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


viewNet : ViewNet msg -> (Msg -> msg) -> NetFormData -> Html msg
viewNet ref toMsg netForm =
    -- TODO option for no net, new net, existing net
    -- todo custom net name input
    -- todo custom net color picker
    div [] [ text "todo" ]
