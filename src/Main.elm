module Main exposing (..)

import Browser
import Date
import Html as H exposing (Html)
import Html.Attributes as Attr
import List.Extra
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    Int


type Msg
    = NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( 1, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( 1, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view _ =
    let
        dates =
            getDatesForMonth Time.Jul 2023
    in
    H.div [ Attr.class "w-72" ]
        [ viewWeekHeader (Maybe.withDefault [] (List.head dates))
        , viewMonth dates
        ]


type alias Week =
    List Date.Date


type alias Month =
    Time.Month


type alias Year =
    Int


getStartDate : Month -> Year -> Date.Date
getStartDate month year =
    Date.fromCalendarDate year month 1


type alias StartOfWeek =
    Time.Weekday


getProperStartDate : StartOfWeek -> Month -> Year -> Date.Date
getProperStartDate startOfWeek month year =
    Date.floor (weekdayToInterval startOfWeek) (Date.fromCalendarDate year month 1)


weekdayToInterval : Time.Weekday -> Date.Interval
weekdayToInterval weekday =
    case weekday of
        Time.Sun ->
            Date.Sunday

        Time.Mon ->
            Date.Monday

        Time.Tue ->
            Date.Tuesday

        Time.Wed ->
            Date.Wednesday

        Time.Thu ->
            Date.Thursday

        Time.Fri ->
            Date.Friday

        Time.Sat ->
            Date.Saturday


getProperEndDate : StartOfWeek -> Month -> Year -> Date.Date
getProperEndDate startOfWeek month year =
    let
        endDate =
            Date.add Date.Months 1 (Date.fromCalendarDate year month 1) |> Date.add Date.Days -1
    in
    Date.ceiling (weekdayToInterval startOfWeek) endDate |> Date.add Date.Days -1


getDatesBetween : Date.Date -> Date.Date -> List Date.Date
getDatesBetween start end =
    Date.range Date.Day 1 start (Date.add Date.Days 1 end)


getMonth : List Date.Date -> List Week
getMonth =
    List.Extra.groupsOf 7


getDatesForMonth : Month -> Year -> List Week
getDatesForMonth month year =
    let
        start =
            getProperStartDate Time.Sun month year

        end =
            getProperEndDate Time.Sun month year

        dates =
            getDatesBetween start end
    in
    getMonth dates


viewDate : Date.Date -> Html Msg
viewDate date =
    H.div [ Attr.class "flex items-center justify-center" ] [ H.text <| Date.format "d" date ]


viewWeek : Week -> Html Msg
viewWeek dates =
    H.div [ Attr.class "grid grid-cols-7 items-center gap-4" ] (List.map viewDate dates)


viewMonth : List Week -> Html Msg
viewMonth weeks =
    H.div [] (List.map viewWeek weeks)


viewWeekHeader : Week -> Html Msg
viewWeekHeader week =
    H.div [ Attr.class "grid grid-cols-7 items-center gap-2" ] <|
        List.map (\date -> H.div [ Attr.class "flex items-center justify-center" ] [ H.text <| Date.format "EEEEE" date ]) week
