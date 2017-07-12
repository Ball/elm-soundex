port module Starbucks exposing (..)

import Array exposing (Array)
import Css
import Dict exposing (..)
import Html exposing (Attribute, Html, div, input, li, node, program, text, ul)
import Html.Attributes exposing (..)
import Html.CssHelpers exposing (withNamespace)
import Html.Events exposing (onInput)
import Random
import Soundex exposing (Soundex, soundex)
import Style exposing (..)
import Task


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias MatchData =
    Dict Soundex (List String)


type alias Model =
    { name : String
    , sndx : Soundex
    , sbuxName : String
    , isBusy : Bool
    , matchData : Maybe MatchData
    , soundexCount : String
    , nameCount : String
    , nameMatches : Maybe MatchData
    , nameMisses : Maybe (List ( Soundex, String ))
    }


initModel : Model
initModel =
    { name = ""
    , sndx = ""
    , sbuxName = ""
    , isBusy = True
    , matchData = Nothing
    , soundexCount = ""
    , nameCount = ""
    , nameMatches = Nothing
    , nameMisses = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


buildMatchData : Array String -> MatchData
buildMatchData words =
    let
        addWord : ( String, Soundex ) -> MatchData -> MatchData
        addWord ( word, key ) data =
            data
                |> Dict.update key (\d -> d |> Maybe.withDefault [] |> (::) word |> Just)
    in
    words
        |> Array.map (\word -> ( word, transform word ))
        |> Array.foldl addWord Dict.empty



-- UPDATE


type Msg
    = NameChange String
    | GotRandom Int
    | NewWords (Array.Array String)
    | NewNames (Array.Array String)
    | CompiledLookup MatchData
    | CompiledNames MatchData



-- | NamesAndWordsBoth


nameMisses : Model -> Maybe (List ( Soundex, String ))
nameMisses model =
    case ( model.matchData, model.nameMatches ) of
        ( Just words, Just names ) ->
            let
                diffs : List ( Soundex, List String )
                diffs =
                    Dict.diff names words |> Dict.toList

                flattenMatch : ( Soundex, List String ) -> List ( Soundex, String )
                flattenMatch ( snx, nms ) =
                    List.map (\n -> ( snx, n )) nms
            in
            diffs
                |> List.concatMap flattenMatch
                |> Just

        _ ->
            Nothing


updateMisses : Model -> Model
updateMisses model =
    { model | nameMisses = nameMisses model }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompiledLookup matchData ->
            ( { model
                | isBusy = False
                , matchData = Maybe.Just matchData
                , soundexCount = toString <| Dict.size matchData
              }
                |> updateMisses
            , Cmd.none
            )

        CompiledNames matchData ->
            ( { model
                | nameMatches = Maybe.Just matchData
                , nameCount = toString <| Dict.size matchData
              }
                |> updateMisses
            , Cmd.none
            )

        NewWords words ->
            ( model
            , Task.perform (\ws -> CompiledLookup <| buildMatchData ws) (Task.succeed words)
            )

        NewNames names ->
            ( model
            , Task.perform (\ws -> CompiledNames <| buildMatchData ws) (Task.succeed names)
            )

        NameChange "" ->
            ( { model | name = "", sndx = "", sbuxName = "" }
            , Cmd.none
            )

        NameChange newName ->
            let
                soundex =
                    transform newName

                optionCount =
                    model.matchData
                        |> Maybe.map (\matchData -> howMutch matchData soundex)
                        |> Maybe.withDefault 0
            in
            if optionCount == 0 then
                ( { model | name = newName, sndx = soundex, sbuxName = soundex }
                , Cmd.none
                )
            else
                ( { model | name = newName, sndx = transform newName }
                , Random.generate GotRandom (Random.int 0 (optionCount - 1))
                )

        GotRandom index ->
            ( { model | sbuxName = lookup model.matchData model.sndx index }
            , Cmd.none
            )


lookup : Maybe MatchData -> Soundex -> Int -> String
lookup match sndx index =
    case match of
        Just match ->
            match
                |> Dict.get sndx
                |> Maybe.andThen (\ls -> ls |> List.drop (index - 1) |> List.head)
                |> Maybe.withDefault sndx

        Nothing ->
            sndx


transform : String -> Soundex
transform =
    Soundex.soundex


howMutch : MatchData -> Soundex -> Int
howMutch match soundex =
    Dict.get soundex match
        |> Maybe.withDefault []
        |> List.length



-- SUBSCRIPTIONS


port setWords : (Array String -> msg) -> Sub msg


port setNames : (Array String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ setWords NewWords
        , setNames NewNames
        ]



-- VIEW
-- TODO : busy spinner / background number
-- TODO : include a number of names that miss


{ id, class, classList } =
    withNamespace "sbux"


styleOutput : { warnings : List String, css : String }
styleOutput =
    Css.compile [ css ]


view : Model -> Html Msg
view model =
    div []
        [ node "style"
            [ Html.Attributes.type_ "text/css" ]
            [ text styleOutput.css ]
        , cup model
        ]


cup : Model -> Html Msg
cup model =
    case ( model.matchData, model.nameMatches ) of
        ( Nothing, Nothing ) ->
            div [] [ text "Please wait, we are building our corpus" ]

        ( Nothing, _ ) ->
            div [] [ text "Please wait, we are building our corbus" ]

        ( _, _ ) ->
            div [ class [ Wrapper ] ]
                [ input [ placeholder "Your name", onInput NameChange, class [ OrderName ] ] []
                , div [ class [ Cup ] ] [ text model.sbuxName ]
                , div [] [ text <| "Your sound index: " ++ model.sndx ]
                , soundIndexLabel model
                , nameIndexLabel model
                , missesLabel model
                , uniqueNames model
                ]


uniqueNames : Model -> Html Msg
uniqueNames model =
    case model.nameMisses of
        Nothing ->
            div [] []

        Just misses ->
            ul [] (List.map (\( s, n ) -> li [] [ text <| s ++ ": " ++ n ]) misses)


missesLabel : Model -> Html Msg
missesLabel model =
    case model.nameMisses of
        Nothing ->
            div [] [ text "---" ]

        Just misses ->
            div [] [ text <| "There are " ++ (toString <| List.length misses) ++ " names without matching words" ]


nameIndexLabel : Model -> Html Msg
nameIndexLabel model =
    case model.nameMatches of
        Nothing ->
            div [] []

        Just matchData ->
            div [] [ text <| "Cataloging " ++ (toString <| Dict.size matchData) ++ " name sounds" ]


soundIndexLabel : Model -> Html Msg
soundIndexLabel model =
    case model.matchData of
        Nothing ->
            div [] []

        Just matchData ->
            div [] [ text <| "Cataloging " ++ (toString <| Dict.size matchData) ++ " sound indexs" ]
