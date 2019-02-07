port module Main exposing (Habit, Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import FeatherIcons as Icons
import Html exposing (..)
import Html.Attributes as Attributes exposing (class)
import Html.Events as Events
import Http
import Iso8601 as Iso
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (object)
import Task
import Time
import Url
import Utils



-- ========================================================================== --
--                                    PORTS                                   --
-- ========================================================================== --


port writeToLocalStorage : Encode.Value -> Cmd msg



-- ========================================================================== --
--                                    MODEL                                   --
-- ========================================================================== --


type alias Flags =
    { apiUrl : String
    , userId : Maybe String
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , apiUrl : String
    , habits : List Habit
    , entries : List Entry
    , email : String
    , newHabitName : String
    , newHabitCategory : String
    , user : Maybe User
    , userId : Maybe String
    , error : Maybe String
    , showNewHabit : Bool
    , loading : Bool
    , time : Time.Posix
    , timezone : Time.Zone
    , route : Route
    }


type alias Entry =
    { id : Int
    , habit : Habit
    , created : Time.Posix
    , unixCreated : Time.Posix
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { url = url
      , key = key
      , apiUrl = flags.apiUrl
      , habits = []
      , entries = []
      , email = ""
      , newHabitName = ""
      , newHabitCategory = ""
      , user = Nothing
      , error = Nothing
      , showNewHabit = False
      , loading = True
      , userId = flags.userId
      , time = Time.millisToPosix 0
      , timezone = Time.utc
      , route = HabitsRoute
      }
    , Cmd.batch [ Task.perform SetTimeZone Time.here, Task.perform Tick Time.now ]
    )


type alias Habit =
    { id : Int
    , name : String
    , category : String
    , weight : Int
    }


type alias NewHabit =
    { name : String
    , category : String
    , weight : Int
    }


type alias User =
    { id : Int
    , name : String
    , email : String
    , habits : List Habit
    }


type Route
    = HabitsRoute
    | ReportRoute


habitIsCompleted : Habit -> List Entry -> Bool
habitIsCompleted habit entries =
    entries
        |> List.any (\entry -> entry.habit.id == habit.id)



-- ========================================================================== --
--                                   UPDATE                                   --
-- ========================================================================== --


type Msg
    = NoOp
    | AddHabit
    | HandleNewHabitChange String
    | HandleNewHabitCategoryChange String
    | GotUsers (Result Http.Error (List User))
    | GotUser (Result Http.Error User)
    | GotEntries (Result Http.Error (List Entry))
    | ChangeEmail String
    | OnError String
    | ToggleHabit Habit
    | GotHabitEntryCreated (Result Http.Error Entry)
    | DeletedEntry (Result Http.Error Entry)
    | ToggleShowNewHabit
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | Login String
    | Logout
    | GotPing (Result Http.Error ())
    | Tick Time.Posix
    | SetTimeZone Time.Zone
    | GotHabitCreated (Result Http.Error Habit)
    | DismissError
    | RouteChanged Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeEmail email ->
            ( { model | email = email }, Cmd.none )

        AddHabit ->
            let
                newHabit =
                    { name = model.newHabitName, category = model.newHabitCategory, weight = 1 }
            in
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just u ->
                    ( model, createHabit model.apiUrl newHabit u.id )

        GotHabitCreated (Ok habit) ->
            case model.user of
                Nothing ->
                    ( { model | error = Just "User not authenticated" }, Cmd.none )

                Just u ->
                    let
                        habits =
                            List.append model.habits [ habit ]
                    in
                    ( { model | habits = habits, error = Nothing, showNewHabit = False }
                    , getUserHabitEntriesToday model.apiUrl u.id model.time model.timezone
                    )

        GotHabitCreated (Err _) ->
            ( { model | error = Just "Failed to create new habit" }, Cmd.none )

        HandleNewHabitChange newHabitName ->
            ( { model | newHabitName = newHabitName }, Cmd.none )

        HandleNewHabitCategoryChange category ->
            ( { model | newHabitCategory = category }, Cmd.none )

        GotUsers result ->
            case result of
                Ok users ->
                    case List.head users of
                        Just user ->
                            ( { model | user = Just user, habits = user.habits, loading = False }
                            , Cmd.batch
                                [ getUserHabitEntriesToday model.apiUrl user.id model.time model.timezone
                                , writeToLocalStorage (Encode.object [ ( "userId", Encode.string (String.fromInt user.id) ) ])
                                ]
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Failed to fetch users", loading = False }, Cmd.none )

        GotUser result ->
            case result of
                Ok user ->
                    ( { model | user = Just user, habits = user.habits, loading = False }
                    , Cmd.batch
                        [ getUserHabitEntriesToday model.apiUrl user.id model.time model.timezone
                        , writeToLocalStorage (Encode.object [ ( "userId", Encode.string (String.fromInt user.id) ) ])
                        ]
                    )

                Err _ ->
                    ( { model | error = Just "Failed to fetch users", loading = False }, Cmd.none )

        GotEntries result ->
            case result of
                Ok entries ->
                    ( { model | entries = entries }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        ToggleHabit habit ->
            let
                completed =
                    habitIsCompleted habit model.entries
            in
            ( model
            , if completed then
                removeHabitEntry model.apiUrl habit model.entries

              else
                createHabitEntry model.apiUrl habit model.time
            )

        GotHabitEntryCreated result ->
            case result of
                Err _ ->
                    ( { model | error = Just "Failed to create entry" }, Cmd.none )

                Ok entry ->
                    ( { model | entries = entry :: model.entries }, Cmd.none )

        DeletedEntry result ->
            case result of
                Err _ ->
                    ( { model | error = Just "Failed to remove entry" }, Cmd.none )

                Ok entry ->
                    let
                        entries =
                            List.filter (\e -> e.id /= entry.id) model.entries
                    in
                    ( { model | entries = entries }, Cmd.none )

        ToggleShowNewHabit ->
            ( { model | showNewHabit = not model.showNewHabit }, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        Login email ->
            ( model, getUserByEmail model.apiUrl email )

        Logout ->
            ( { model | user = Nothing }, writeToLocalStorage (Encode.object [ ( "userId", Encode.null ) ]) )

        GotPing result ->
            case result of
                Err _ ->
                    ( { model | loading = False, error = Just "Can not reach server" }, Cmd.none )

                Ok _ ->
                    ( { model | loading = False }
                    , case model.userId of
                        Just id ->
                            getUserById model.apiUrl id

                        Nothing ->
                            Cmd.none
                    )

        Tick time ->
            ( { model | time = time }
            , case model.user of
                Just user ->
                    getUserHabitEntriesToday model.apiUrl user.id time model.timezone

                Nothing ->
                    Cmd.none
            )

        SetTimeZone zone ->
            ( { model | timezone = zone }
            , pingApi model.apiUrl
            )

        DismissError ->
            ( { model | error = Nothing }, Cmd.none )

        RouteChanged route ->
            ( { model | route = route }, Cmd.none )



-- ========================================================================== --
--                                    HTTP                                    --
-- ========================================================================== --


getUserByEmail : String -> String -> Cmd Msg
getUserByEmail apiUrl email =
    Http.get
        { url = apiUrl ++ "/users?email=" ++ email
        , expect = Http.expectJson GotUsers usersDecoder
        }


getUserById : String -> String -> Cmd Msg
getUserById apiUrl id =
    Http.get
        { url = apiUrl ++ "/users/" ++ id
        , expect = Http.expectJson GotUser userDecoder
        }


getUserHabitEntriesToday : String -> Int -> Time.Posix -> Time.Zone -> Cmd Msg
getUserHabitEntriesToday apiUrl userId time zone =
    let
        timeMs =
            Time.posixToMillis time

        timeEnd =
            timeMs + Utils.timeTilEndOfDay time zone

        timeStart =
            timeMs - Utils.timeTilStartOfDay time zone
    in
    Http.get
        { url =
            apiUrl
                ++ "/entries/between?userId="
                ++ String.fromInt userId
                ++ "&start="
                ++ String.fromInt timeStart
                ++ "&end="
                ++ String.fromInt timeEnd
        , expect = Http.expectJson GotEntries entriesDecoder
        }


createHabitEntry : String -> Habit -> Time.Posix -> Cmd Msg
createHabitEntry apiUrl habit time =
    Http.post
        { url = apiUrl ++ "/entries"
        , body = Http.jsonBody (entryEncoder habit time)
        , expect = Http.expectJson GotHabitEntryCreated entryDecoder
        }


removeHabitEntry : String -> Habit -> List Entry -> Cmd Msg
removeHabitEntry apiUrl habit entries =
    let
        entry =
            entries
                |> List.filter (\e -> e.habit.id == habit.id)
                |> List.head
    in
    case entry of
        Nothing ->
            Cmd.none

        Just habitEntry ->
            Http.request
                { method = "DELETE"
                , url = apiUrl ++ "/entries/" ++ String.fromInt habitEntry.id
                , expect = Http.expectJson DeletedEntry entryDecoder
                , headers = []
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                }


createHabit : String -> NewHabit -> Int -> Cmd Msg
createHabit apiUrl newHabit userId =
    Http.post
        { url = apiUrl ++ "/habits"
        , body = Http.jsonBody (habitEncoder newHabit userId)
        , expect = Http.expectJson GotHabitCreated habitDecoder
        }


pingApi : String -> Cmd Msg
pingApi apiUrl =
    Http.get
        { url = apiUrl ++ "/ping"
        , expect = Http.expectWhatever GotPing
        }



-- SERIALIZATION


usersDecoder : Decoder (List User)
usersDecoder =
    list userDecoder


habitsDecoder : Decoder (List Habit)
habitsDecoder =
    list habitDecoder


habitDecoder : Decoder Habit
habitDecoder =
    Decode.succeed Habit
        |> required "id" int
        |> required "name" string
        |> required "category" string
        |> required "weight" int


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "id" int
        |> required "name" string
        |> required "email" string
        |> required "habits" habitsDecoder


entriesDecoder : Decoder (List Entry)
entriesDecoder =
    list entryDecoder


secondsToPosix : Int -> Time.Posix
secondsToPosix seconds =
    Time.millisToPosix (seconds * 1000)


timeDecoder : Decoder Time.Posix
timeDecoder =
    Decode.map secondsToPosix int


entryDecoder : Decoder Entry
entryDecoder =
    Decode.succeed Entry
        |> required "id" int
        |> required "habit" habitDecoder
        |> required "created" Iso.decoder
        |> required "unixCreated" timeDecoder


entryEncoder : Habit -> Time.Posix -> Encode.Value
entryEncoder habit time =
    let
        timeSeconds =
            Time.posixToMillis time // 1000
    in
    Encode.object [ ( "habit", Encode.int habit.id ), ( "unixCreated", Encode.int timeSeconds ) ]


habitEncoder : NewHabit -> Int -> Encode.Value
habitEncoder newHabit userId =
    Encode.object
        [ ( "name", Encode.string newHabit.name )
        , ( "category", Encode.string newHabit.category )
        , ( "weight", Encode.int newHabit.weight )
        , ( "user", Encode.int userId )
        ]



-- ========================================================================== --
--                                    VIEW                                    --
-- ========================================================================== --


view : Model -> Browser.Document Msg
view model =
    { title = "Habits"
    , body =
        [ div [ class "container" ]
            [ renderError model.error
            , h1 [ class "title" ] [ text "Habits" ]
            , if model.loading then
                div [] [ text "Loading..." ]

              else
                div [ class "app" ]
                    (case model.route of
                        HabitsRoute ->
                            renderHabitsPage model

                        ReportRoute ->
                            renderReportRoute model
                    )
            ]
        ]
    }


renderHabitsPage : Model -> List (Html Msg)
renderHabitsPage model =
    [ case model.user of
        Nothing ->
            form [ Events.onSubmit (Login model.email) ]
                [ p [] [ text "Enter your email address to log in:" ]
                , input [ Attributes.placeholder "Email address", class "input", Events.onInput ChangeEmail, Attributes.value model.email ] []
                , div [ class "separator" ] []
                , button [ Attributes.type_ "submit" ] [ text "Go" ]
                ]

        Just _ ->
            div []
                [ currentDay model.time model.timezone
                , h2 [ class "point-counter" ]
                    [ text ((model.entries |> List.length |> String.fromInt) ++ " point(s) earned today")
                    ]
                , renderHabits model.habits model.entries
                , if model.showNewHabit then
                    newHabitForm model

                  else
                    div [ class "footer" ]
                        [ Icons.logOut |> Icons.toHtml [ Events.onClick Logout ]
                        , Icons.plusSquare |> Icons.toHtml [ Events.onClick ToggleShowNewHabit ]
                        , Icons.pieChart |> Icons.toHtml [ Events.onClick (RouteChanged ReportRoute) ]
                        ]
                ]
    ]


renderReportRoute : Model -> List (Html Msg)
renderReportRoute model =
    [ div [] [ text "REPORT PAGE" ]
    , div [ class "footer" ]
        [ Icons.calendar |> Icons.toHtml [ Events.onClick (RouteChanged HabitsRoute) ]
        ]
    ]


currentDay : Time.Posix -> Time.Zone -> Html Msg
currentDay time zone =
    let
        month =
            Utils.monthToString (Time.toMonth zone time)

        day =
            String.fromInt (Time.toDay zone time)

        yesterday =
            Time.posixToMillis time
                - 86400000
                |> Time.millisToPosix

        tomorrow =
            Time.posixToMillis time
                + 86400000
                |> Time.millisToPosix
    in
    div [ class "day-container" ]
        [ div [ class "day-selector-button", Events.onClick (Tick yesterday) ] [ Icons.chevronsLeft |> Icons.toHtml [] ]
        , text (month ++ " " ++ day)
        , div [ class "day-selector-button", Events.onClick (Tick tomorrow) ] [ Icons.chevronsRight |> Icons.toHtml [] ]
        ]


renderError : Maybe String -> Html Msg
renderError maybeError =
    case maybeError of
        Nothing ->
            span [] []

        Just error ->
            span [ class "error-container" ]
                [ div [ class "error" ]
                    [ span [] [ text error ]
                    ]
                , div [ Events.onClick DismissError, class "dismiss-error-container" ]
                    [ span [] [ text "Dismiss" ]
                    ]
                ]


renderHabits : List Habit -> List Entry -> Html Msg
renderHabits habits entries =
    div [ class "habits-container" ] (habits |> List.map (renderHabit entries))


renderHabit : List Entry -> Habit -> Html Msg
renderHabit entries habit =
    div
        [ class
            ("habit"
                ++ (if habitIsCompleted habit entries then
                        " completed"

                    else
                        " pending"
                   )
            )
        , Events.onClick (ToggleHabit habit)
        ]
        [ h3
            []
            [ text habit.name ]
        ]


newHabitForm : Model -> Html Msg
newHabitForm model =
    form [ class "new-habit-form", Events.onSubmit AddHabit ]
        [ input
            [ Attributes.placeholder "Habit"
            , class "input"
            , Attributes.value model.newHabitName
            , Events.onInput HandleNewHabitChange
            ]
            []
        , input
            [ Attributes.placeholder "Category"
            , class "input"
            , Attributes.value model.newHabitCategory
            , Events.onInput HandleNewHabitCategoryChange
            ]
            []
        , div [ class "new-habit-buttons" ]
            [ button [ Attributes.type_ "button", class "cancel", Events.onClick ToggleShowNewHabit ] [ text "Cancel" ]
            , button [ Attributes.type_ "submit" ] [ text "Create" ]
            ]
        ]



-- ========================================================================== --
--                                SUBSCRIPTIONS                               --
-- ========================================================================== --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- ========================================================================== --
--                                   PROGRAM                                  --
-- ========================================================================== --


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
