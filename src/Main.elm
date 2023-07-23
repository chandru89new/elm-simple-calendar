module Main exposing (..)

import Browser
import Date
import Html as H exposing (Html)
import Html.Attributes as Attr
import Time
import Utils


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


view : Model -> Html Msg
view _ =
    H.div [] <|
        [ viewMonth (Utils.getDatesToRender Time.Sun Time.Jul 2023) ]


viewDay : Utils.CalendarDate -> Html Msg
viewDay { date, dateBelongsToCurrentMonth } =
    H.div
        [ Attr.class "flex items-center justify-center"
        , Attr.class
            (if dateBelongsToCurrentMonth then
                ""

             else
                "opacity-10"
            )
        ]
        [ H.text <| Date.format "d" date ]


viewWeek : List Utils.CalendarDate -> List (Html Msg)
viewWeek dates =
    viewRow (List.map viewDay dates)


viewMonth : List (List Utils.CalendarDate) -> Html Msg
viewMonth weeks =
    H.div
        [ Attr.class "p-4 border rounded w-72 space-y-2"
        ]
    <|
        [ viewBox <|
            List.concat
                [ viewMonthHeader <| Maybe.withDefault [] (List.head weeks)
                , List.concatMap viewWeek weeks
                ]
        ]


viewMonthHeader : List Utils.CalendarDate -> List (Html Msg)
viewMonthHeader dates =
    viewRow
        (List.map (\item -> H.div [ Attr.class "text-slate-300" ] [ H.text <| Date.format "EEEEE" item.date ]) dates)



-- H.div [ Attr.class "grid grid-cols-7 items-center gap-1" ] <|
--     (getWeekDaysFromWeekDates dates |> List.map (\wkday -> H.div [ Attr.class "flex items-center justify-center opacity-30" ] [ H.text <| Utils.weekdayHead wkday ]))


viewRow : List (Html Msg) -> List (Html Msg)
viewRow =
    List.map
        (\item ->
            H.div
                [ Attr.class "flex items-center justify-center"
                ]
                [ item ]
        )


viewBox : List (Html Msg) -> Html Msg
viewBox rows =
    H.div
        [ Attr.class "grid grid-cols-7 items-center gap-1"
        ]
        rows


getWeekDaysFromWeekDates : List Utils.CalendarDate -> List Date.Weekday
getWeekDaysFromWeekDates dates =
    List.map (\{ date } -> Date.weekday date) dates


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
