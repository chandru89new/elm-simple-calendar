module Utils exposing (..)

import Date
import List.Extra
import Time


getProperStartDate : Time.Weekday -> Date.Date -> Date.Date
getProperStartDate weekday date =
    Date.floor (weekdayToInterval weekday) date


getProperEndDate : Time.Weekday -> Date.Date -> Date.Date
getProperEndDate weekday date =
    Date.ceiling (weekdayToInterval weekday) date
        |> Date.add Date.Days -1


getEndOfMonthDate : Date.Date -> Date.Date
getEndOfMonthDate startDate =
    Date.add Date.Months 1 startDate
        |> Date.add Date.Days -1


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


type alias CalendarDate =
    { date : Date.Date
    , dateBelongsToCurrentMonth : Bool
    }


addMetadataToDate : Date.Month -> Date.Date -> CalendarDate
addMetadataToDate currMonth date =
    { date = date
    , dateBelongsToCurrentMonth = Date.month date == currMonth
    }


getWeekDates : Time.Weekday -> Date.Date -> List (List CalendarDate)
getWeekDates wkday startDate =
    let
        currentMonth =
            Date.month startDate

        endDate =
            getEndOfMonthDate startDate
    in
    List.Extra.groupsOf 7 <|
        List.map (addMetadataToDate currentMonth) <|
            Date.range Date.Day
                1
                (getProperStartDate wkday startDate)
                (getProperEndDate wkday endDate |> Date.add Date.Days 1)


getDatesToRender : Time.Weekday -> Time.Month -> Int -> List (List CalendarDate)
getDatesToRender startOfWeek month year =
    let
        startDate =
            Date.fromCalendarDate year month 1
    in
    getWeekDates startOfWeek startDate


weekdayHead : Time.Weekday -> String
weekdayHead wkday =
    case wkday of
        Time.Sun ->
            "S"

        Time.Mon ->
            "M"

        Time.Tue ->
            "T"

        Time.Wed ->
            "W"

        Time.Thu ->
            "T"

        Time.Fri ->
            "F"

        Time.Sat ->
            "S"


testFormat =
    Date.format "EEEE, d MMMM y"
