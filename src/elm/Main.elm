port module Main exposing (main)

import Bool.Extra exposing (ifElse)
import Browser exposing (element)
import Html exposing (Html, button, div, form, h1, h3, hr, i, input, label, p, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, for, id, placeholder, scope, type_, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as JsonD
import Json.Encode as JsonE
import List.Extra as ListE
import Round


port saveSettings : String -> Cmd msg


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


type alias Flags =
    String


main : Program Flags Model Msg
main =
    element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { usage : Int
    , showSettings : Bool
    , usageRateMap : List UsageRateMapEntry
    , vatPercentage : Float
    , demandCharge : Float
    }


type Msg
    = UsageUpdate String
    | ToggleSettingsVisibility
    | AddUsageRateMapEntry
    | RemoveUsageRateMapEntry Int
    | UpdateUsageRateMapEntryUsage Int String
    | UpdateUsageRateMapEntryRate Int String
    | UpdateDemandCharge String
    | UpdateVatPercentage String


initModel : Model
initModel =
    { usage = 0
    , showSettings = False
    , usageRateMap =
        [ UsageRateMapEntry 50 3.75
        , UsageRateMapEntry 75 4.19
        , UsageRateMapEntry 124 5.72
        , UsageRateMapEntry 99 6.0
        , UsageRateMapEntry 99 6.34
        , UsageRateMapEntry 199 9.94
        , UsageRateMapEntry -1 11.46
        ]
    , vatPercentage = 5
    , demandCharge = 70
    }


init : Flags -> ( Model, Cmd Msg )
init settingsJson =
    ( settingsJson |> settingsFromJson
    , Cmd.none
    )


settingsFromJson : String -> Model
settingsFromJson json =
    let
        usageDecoder =
            JsonD.field "usage" JsonD.int

        rateDecoder =
            JsonD.field "rate" JsonD.float

        usageRateMapEntryDecoder =
            JsonD.map2 UsageRateMapEntry usageDecoder rateDecoder

        usageRateMapDecoder =
            JsonD.field "usage_rate_map" (JsonD.list usageRateMapEntryDecoder)

        vatPercentageDecoder =
            JsonD.field "vat_percentage" JsonD.float

        demandChargeDecoder =
            JsonD.field "demand_charge" JsonD.float

        result =
            json |> JsonD.decodeString (JsonD.map3 (Model initModel.usage initModel.showSettings) usageRateMapDecoder vatPercentageDecoder demandChargeDecoder)
    in
    result |> Result.withDefault initModel


settingsToJson : Model -> String
settingsToJson model =
    let
        usageValue =
            JsonE.int

        rateValue =
            JsonE.float

        usageRateMapEntryValue : UsageRateMapEntry -> JsonE.Value
        usageRateMapEntryValue usageRateMapEntry =
            JsonE.object
                [ ( "usage", usageValue usageRateMapEntry.usage )
                , ( "rate", rateValue usageRateMapEntry.rate )
                ]

        usageRateMapValue =
            JsonE.list usageRateMapEntryValue model.usageRateMap

        vatPercentageValue =
            JsonE.float model.vatPercentage

        demandChargeValue =
            JsonE.float model.demandCharge
    in
    JsonE.object
        [ ( "usage_rate_map", usageRateMapValue )
        , ( "vat_percentage", vatPercentageValue )
        , ( "demand_charge", demandChargeValue )
        ]
        |> JsonE.encode 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, shouldSaveSettings ) =
            case msg of
                UsageUpdate value ->
                    ( { model | usage = value |> String.toInt |> Maybe.withDefault 0 }, False )

                ToggleSettingsVisibility ->
                    ( { model | showSettings = not model.showSettings }, False )

                RemoveUsageRateMapEntry index ->
                    ( { model | usageRateMap = model.usageRateMap |> ListE.removeAt index }, True )

                AddUsageRateMapEntry ->
                    ( { model | usageRateMap = model.usageRateMap ++ [ UsageRateMapEntry 0 0 ] |> ListE.swapAt (List.length model.usageRateMap - 1) (List.length model.usageRateMap) }, True )

                UpdateUsageRateMapEntryUsage index usage ->
                    ( { model | usageRateMap = model.usageRateMap |> ListE.updateAt index (\mapping -> { mapping | usage = usage |> String.toInt |> Maybe.withDefault 0 }) }, True )

                UpdateUsageRateMapEntryRate index rate ->
                    ( { model | usageRateMap = model.usageRateMap |> ListE.updateAt index (\mapping -> { mapping | rate = rate |> String.toFloat |> Maybe.withDefault 0 }) }, True )

                UpdateDemandCharge demandCharge ->
                    ( { model | demandCharge = demandCharge |> String.toFloat |> Maybe.withDefault 0 }, True )

                UpdateVatPercentage vatPercentage ->
                    ( { model | vatPercentage = vatPercentage |> String.toFloat |> Maybe.withDefault 0 }, True )
    in
    ( newModel, ifElse (saveSettings <| settingsToJson newModel) Cmd.none shouldSaveSettings )


view : Model -> Html Msg
view model =
    let
        ( billSteps, billSummary ) =
            billFromModel model
    in
    div [ class "container" ]
        -- Heading
        [ h1 [ class "mt-4 mb-3" ] [ text "Electricity Bill Calculator" ]

        -- Electricity usage units
        , input
            [ attribute "aria-label" ".form-control-lg example"
            , class "form-control form-control-lg mb-3"
            , placeholder "Electricity Usage Units (e.g. 256)"
            , type_ "text"
            , onInput UsageUpdate
            ]
            []
        , hr [] []

        -- Show/hide settings
        , button
            [ class "btn btn-outline-primary", type_ "button", onClick ToggleSettingsVisibility ]
            (if model.showSettings then
                [ i [ class "bi bi-dash-lg" ] [], text " Hide Settings" ]

             else
                [ i [ class "bi bi-plus-lg" ] [], text " Show Settings" ]
            )

        -- Settings
        , div
            [ class <| ifElse "" "d-none" model.showSettings ]
            [ hr [] []
            , h1 [] [ text "Settings" ]
            , form []
                [ div [ class "mt-3 mb-3" ]
                    [ label
                        [ class "form-label"
                        , for "demand-charge"
                        ]
                        [ text "Demand Charge (BDT)" ]
                    , input
                        [ class "form-control mb-3"
                        , id "demand-charge"
                        , type_ "text"
                        , on "blur" (targetValue |> JsonD.map UpdateDemandCharge)
                        , value <| Round.round 2 <| model.demandCharge
                        ]
                        []
                    , label
                        [ class "form-label"
                        , for "vat-percentage"
                        ]
                        [ text "VAT (%)" ]
                    , input
                        [ class "form-control"
                        , id "vat-percentage"
                        , type_ "text"
                        , on "blur" (targetValue |> JsonD.map UpdateVatPercentage)
                        , value <| Round.round 2 <| model.vatPercentage
                        ]
                        []
                    ]
                ]
            , hr [] []
            , h3 [ class "mt-3" ] [ text "Usage-Rate Map" ]
            , hr [] []

            -- Rates actions
            , button [ class "btn btn-outline-success", onClick AddUsageRateMapEntry ] [ i [ class "bi bi-folder-plus" ] [], text " Add Entry" ]
            , hr [] []

            -- Rates table
            , table [ class "table table-striped" ]
                [ thead []
                    [ tr []
                        [ th [ scope "col" ]
                            [ text "Steps" ]
                        , th [ scope "col" ]
                            [ text "Usage (Units)" ]
                        , th [ scope "col" ]
                            [ text "Rate (BDT / Unit)" ]
                        , th [ scope "col" ]
                            [ text "Actions" ]
                        ]
                    ]
                , tbody []
                    (model.usageRateMap
                        |> List.indexedMap
                            (\index usageRateMapEntry ->
                                tr []
                                    [ td [ class "align-middle" ]
                                        [ text <| ifElse "Lifeline" (String.fromInt index) (index == 0) ]
                                    , td [ class "align-middle" ]
                                        [ if usageRateMapEntry.usage /= -1 then
                                            input
                                                [ class "form-control"
                                                , value <| String.fromInt usageRateMapEntry.usage
                                                , on "blur" (targetValue |> JsonD.map (UpdateUsageRateMapEntryUsage index))
                                                ]
                                                []

                                          else
                                            text "Onward"
                                        ]
                                    , td []
                                        [ input
                                            [ class "form-control"
                                            , value <| Round.round 2 usageRateMapEntry.rate
                                            , on "blur" (targetValue |> JsonD.map (UpdateUsageRateMapEntryRate index))
                                            ]
                                            []
                                        ]
                                    , td []
                                        [ if usageRateMapEntry.usage /= -1 && index /= 0 then
                                            button
                                                [ class "btn btn-outline-danger", onClick <| RemoveUsageRateMapEntry index ]
                                                [ i [ class "bi bi-trash" ]
                                                    []
                                                ]

                                          else
                                            text ""
                                        ]
                                    ]
                            )
                    )
                ]
            ]
        , hr [] []

        -- Bill table
        , h1 []
            [ text "Bill" ]
        , table
            [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [ scope "col" ]
                        [ text "Usage (Units)" ]
                    , th [ scope "col" ]
                        [ text "Rate (BDT / Unit)" ]
                    , th [ scope "col" ]
                        [ text "Charge (BDT)" ]
                    ]
                ]
            , tbody [] billSteps
            ]
        , billSummary
        ]


type alias UsageRateMapEntry =
    { usage : Int
    , rate : Float
    }


charge : UsageRateMapEntry -> Float
charge { usage, rate } =
    toFloat usage * rate


billFromModel : Model -> ( List (Html Msg), Html Msg )
billFromModel model =
    let
        lifeLineMapping =
            model.usageRateMap |> List.head |> Maybe.withDefault (UsageRateMapEntry 0 0)

        steps : List UsageRateMapEntry
        steps =
            if List.length model.usageRateMap < 2 then
                []

            else if model.usage <= lifeLineMapping.usage then
                List.singleton <| lifeLineMapping

            else
                model.usageRateMap
                    |> List.tail
                    |> Maybe.withDefault []
                    |> List.foldl
                        (\mapping ( usage, steps_ ) ->
                            if usage > 0 then
                                if mapping.usage == -1 then
                                    ( 0, steps_ ++ (List.singleton <| UsageRateMapEntry usage mapping.rate) )

                                else
                                    let
                                        reducedUsage =
                                            usage - mapping.usage

                                        newUsage =
                                            if reducedUsage < 0 then
                                                0

                                            else
                                                reducedUsage

                                        newStep =
                                            UsageRateMapEntry (usage - newUsage) mapping.rate
                                    in
                                    ( newUsage, steps_ ++ List.singleton newStep )

                            else
                                ( 0, steps_ )
                        )
                        ( model.usage, [] )
                    |> Tuple.second

        stepsHtml =
            steps
                |> List.map
                    (\{ usage, rate } ->
                        tr []
                            [ td []
                                [ text <| String.fromInt <| usage
                                ]
                            , td []
                                [ text <| Round.round 2 <| rate
                                ]
                            , td []
                                [ text <| Round.round 2 <| charge <| UsageRateMapEntry usage rate
                                ]
                            ]
                    )

        summary : Html Msg
        summary =
            let
                subTotal =
                    steps |> List.foldl (\step acc -> acc + charge step) 0

                vat =
                    (model.vatPercentage / 100) * subTotal
            in
            div []
                [ p
                    []
                    [ strong
                        []
                        [ text "Sub-total (BDT): " ]
                    , text <| Round.round 2 <| subTotal
                    ]
                , p []
                    [ strong
                        []
                        [ text <| String.concat [ "VAT (", String.fromFloat model.vatPercentage, "%): " ] ]
                    , text <| Round.round 2 <| vat
                    ]
                , p []
                    [ strong
                        []
                        [ text "Demand Charge (BDT): " ]
                    , text <| Round.round 2 <| model.demandCharge
                    ]
                , p []
                    [ strong
                        []
                        [ text "Total (BDT): " ]
                    , text <| Round.round 2 <| subTotal + vat + model.demandCharge
                    ]
                ]
    in
    ( stepsHtml, summary )
