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
        year =
            2023

        months =
            [ Time.Jan
            , Time.Feb
            , Time.Mar
            , Time.Apr
            , Time.May
            , Time.Jun
            , Time.Jul
            , Time.Aug
            , Time.Sep
            , Time.Oct
            , Time.Nov
            , Time.Dec
            ]
    in
    H.div [ Attr.class "p-8 grid grid-cols-4 gap-4 items-stretch" ]
        (List.map (\month -> viewMonthBox month year) months)


type alias CalendarDate =
    { date : Date.Date, dateInCurrentMonth : Bool }


type alias Week =
    List CalendarDate


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

        endDateIsStartOfWeek =
            Date.weekday endDate == startOfWeek
    in
    if endDateIsStartOfWeek then
        Date.add Date.Days 7 endDate

    else
        Date.ceiling (weekdayToInterval startOfWeek) endDate |> Date.add Date.Days -1


getDatesBetween : Date.Date -> Date.Date -> List Date.Date
getDatesBetween start end =
    Date.range Date.Day 1 start (Date.add Date.Days 1 end)


getMonth : List CalendarDate -> List Week
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
                |> List.map (\date -> { date = date, dateInCurrentMonth = Date.month date == month })
    in
    getMonth dates


viewDate : CalendarDate -> Html Msg
viewDate { date, dateInCurrentMonth } =
    H.div
        [ Attr.class "flex items-center justify-center"
        , Attr.class
            (if dateInCurrentMonth then
                ""

             else
                "opacity-0"
            )
        ]
        [ H.text <| Date.format "d" date ]


viewWeek : Week -> List (Html Msg)
viewWeek dates =
    List.map viewDate dates


viewMonth : List Week -> List (Html Msg)
viewMonth weeks =
    List.concatMap viewWeek weeks


viewWeekHeader : Week -> List (Html Msg)
viewWeekHeader week =
    List.map (\{ date } -> H.div [ Attr.class "flex items-center justify-center opacity-40" ] [ H.text <| Date.format "EEEEE" date ]) week


viewBox : List (Html Msg) -> Html Msg
viewBox =
    H.div [ Attr.class "grid grid-cols-7 gap-x-2 gap-y-4 items-center" ]


viewItem : List (Html Msg) -> Html Msg
viewItem =
    H.div [ Attr.class "flex items-center justify-center" ]


viewMonthBox : Month -> Year -> Html Msg
viewMonthBox month year =
    let
        dates =
            getDatesForMonth month year
    in
    H.div [ Attr.class "rounded-xl p-4 shadow-xl" ]
        [ H.div [ Attr.class "p-2 text-center" ] [ H.text (Date.format "MMMM y" (Date.fromCalendarDate year month 1)) ]
        , viewBox <| List.concat [ viewWeekHeader (Maybe.withDefault [] (List.head dates)), viewMonth dates ]
        ]
