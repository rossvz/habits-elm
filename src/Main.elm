port module Main exposing (Habit, Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Dict
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
import Time.Extra
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
    , loginPassword : String
    , registerName : String
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
      , loginPassword = ""
      , registerName = ""
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
    | RegisterRoute


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
    | ChangePassword String
    | ChangeRegisterName String
    | OnError String
    | ToggleHabit Habit
    | GotHabitEntryCreated (Result Http.Error Entry)
    | DeletedEntry (Result Http.Error Entry)
    | ToggleShowNewHabit
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | Login String String
    | Register String String String
    | Logout
    | GotPing (Result Http.Error ())
    | Tick Time.Posix
    | SetTimeZone Time.Zone
    | GotHabitCreated (Result Http.Error Habit)
    | DismissError
    | RouteChanged Route
    | CheckTokenExp Msg
    | DoStuff
    | GotTokenResponse Msg (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeEmail email ->
            ( { model | email = email }, Cmd.none )

        ChangePassword password ->
            ( { model | loginPassword = password }, Cmd.none )

        ChangeRegisterName name ->
            ( { model | registerName = name }, Cmd.none )

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
                    ( { model
                        | habits = habits
                        , error = Nothing
                        , showNewHabit = False
                        , newHabitName = ""
                        , newHabitCategory = ""
                      }
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
                    ( { model
                        | user = Just user
                        , email = ""
                        , loginPassword = ""
                        , habits = user.habits
                        , loading = False
                        , route = HabitsRoute
                      }
                    , Cmd.batch
                        [ getUserHabitEntriesToday model.apiUrl user.id model.time model.timezone
                        , writeToLocalStorage (Encode.object [ ( "userId", Encode.string (String.fromInt user.id) ) ])
                        ]
                    )

                Err _ ->
                    ( { model | loading = False, route = RegisterRoute }, Cmd.none )

        GotEntries (Ok entries) ->
            ( { model | entries = entries }, Cmd.none )

        GotEntries (Err _) ->
            ( { model | entries = [] }, Cmd.none )

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

        Login email password ->
            ( model, login model.apiUrl email password )

        Register name email password ->
            ( model, register model.apiUrl name email password )

        Logout ->
            ( { model | user = Nothing }, writeToLocalStorage (Encode.object [ ( "userId", Encode.null ) ]) )

        GotPing result ->
            case result of
                Err _ ->
                    ( { model | loading = False, error = Just "Can not reach server" }, Cmd.none )

                Ok _ ->
                    case model.userId of
                        Just id ->
                            ( model, getUserById model.apiUrl id )

                        Nothing ->
                            ( { model | loading = False }, Cmd.none )

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
            let
                cmd =
                    case model.user of
                        Just user ->
                            case route of
                                HabitsRoute ->
                                    getUserHabitEntriesToday model.apiUrl user.id model.time model.timezone

                                ReportRoute ->
                                    getUserHabitEntriesThisWeek model.apiUrl user.id model.time model.timezone

                                _ ->
                                    Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | route = route }, cmd )

        CheckTokenExp message ->
            let
                token_exp =
                    1551802125000

                now_ms =
                    Time.posixToMillis model.time
            in
            ( model
            , if now_ms > token_exp then
                refreshToken (model.apiUrl ++ "/ping") message

              else
                Cmd.none
            )

        GotTokenResponse m (Ok _) ->
            model |> update m

        GotTokenResponse _ (Err err) ->
            let
                _ =
                    Debug.log "error" err
            in
            ( model, Cmd.none )

        DoStuff ->
            ( model
            , case model.user of
                Just user ->
                    getUserHabitEntriesToday model.apiUrl user.id model.time model.timezone

                Nothing ->
                    Cmd.none
            )



-- ========================================================================== --
--                                    HTTP                                    --
-- ========================================================================== --


refreshToken : String -> Msg -> Cmd Msg
refreshToken url msg =
    Http.get
        { url = url
        , expect = Http.expectString (GotTokenResponse msg)
        }


login : String -> String -> String -> Cmd Msg
login apiUrl email password =
    Http.post
        { url = apiUrl ++ "/login"
        , body = Http.jsonBody (loginEncoder email password)
        , expect = Http.expectJson GotUser userDecoder
        }


register : String -> String -> String -> String -> Cmd Msg
register apiUrl name email password =
    Http.post
        { url = apiUrl ++ "/register"
        , body = Http.jsonBody (registerEncoder name email password)
        , expect = Http.expectJson GotUser userDecoder
        }


getUserById : String -> String -> Cmd Msg
getUserById apiUrl id =
    Http.get
        { url = apiUrl ++ "/users/" ++ id
        , expect = Http.expectJson GotUser userDecoder
        }


getUserHabitEntriesInterval : String -> Int -> Time.Posix -> Time.Posix -> Cmd Msg
getUserHabitEntriesInterval apiUrl userId start end =
    let
        startMs =
            Time.posixToMillis start // 1000

        endMs =
            Time.posixToMillis end // 1000
    in
    Http.get
        { url =
            apiUrl
                ++ "/entries/between?userId="
                ++ String.fromInt userId
                ++ "&start="
                ++ String.fromInt startMs
                ++ "&end="
                ++ String.fromInt endMs
        , expect = Http.expectJson GotEntries entriesDecoder
        }


getUserHabitEntriesToday : String -> Int -> Time.Posix -> Time.Zone -> Cmd Msg
getUserHabitEntriesToday apiUrl userId time zone =
    let
        timeEnd =
            Time.Extra.ceiling Time.Extra.Day zone time

        timeStart =
            Time.Extra.floor Time.Extra.Day zone time
    in
    getUserHabitEntriesInterval apiUrl userId timeStart timeEnd


getUserHabitEntriesThisWeek : String -> Int -> Time.Posix -> Time.Zone -> Cmd Msg
getUserHabitEntriesThisWeek apiUrl userId time zone =
    let
        timeEnd =
            Time.Extra.ceiling Time.Extra.Day zone time

        timeStart =
            Time.Extra.floor Time.Extra.Week zone time
    in
    getUserHabitEntriesInterval apiUrl userId timeStart timeEnd


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


loginEncoder : String -> String -> Encode.Value
loginEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


registerEncoder : String -> String -> String -> Encode.Value
registerEncoder name email password =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


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
            , if model.loading then
                div [] [ text "Loading..." ]

              else
                div [ class "app" ]
                    (case model.route of
                        HabitsRoute ->
                            renderHabitsPage model

                        ReportRoute ->
                            renderReportRoute model

                        RegisterRoute ->
                            renderRegisterRoute model
                    )
            ]
        ]
    }


renderHabitsPage : Model -> List (Html Msg)
renderHabitsPage model =
    [ case model.user of
        Nothing ->
            form [ Events.onSubmit (Login model.email model.loginPassword) ]
                [ p [] [ text "Enter your email address to log in:" ]
                , input [ Attributes.placeholder "Email address", class "input", Events.onInput ChangeEmail, Attributes.value model.email ] []
                , input [ Attributes.placeholder "Password", Attributes.type_ "password", class "input", Events.onInput ChangePassword, Attributes.value model.loginPassword ] []
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
                        [ div [ Attributes.class "labelled-icon" ]
                            [ Icons.logOut |> Icons.toHtml [ Events.onClick Logout ]
                            , text "Logout"
                            ]
                        , div [ Attributes.class "labelled-icon" ]
                            [ Icons.plusSquare |> Icons.toHtml [ Events.onClick ToggleShowNewHabit ]
                            , text "New Habit"
                            ]
                        , div [ Attributes.class "labelled-icon" ]
                            [ Icons.pieChart |> Icons.toHtml [ Events.onClick (RouteChanged ReportRoute) ]
                            , text "Reports"
                            ]
                        ]
                ]
    ]


renderReportRoute : Model -> List (Html Msg)
renderReportRoute model =
    let
        foldByName =
            \entry acc ->
                let
                    newValue =
                        case Dict.get entry.habit.name acc of
                            Nothing ->
                                1

                            Just v ->
                                v + 1
                in
                Dict.insert entry.habit.name newValue acc

        entriesCountByName =
            List.foldl foldByName Dict.empty model.entries
    in
    [ div []
        [ h2 [] [ text ((model.entries |> List.length |> String.fromInt) ++ " points this week") ]
        , div []
            (Dict.toList entriesCountByName
                |> List.map
                    (\( key, val ) ->
                        div [ class "report-row" ]
                            [ span [] [ text key ]
                            , span [] [ text (String.fromInt val) ]
                            ]
                    )
            )
        ]
    , div [ class "footer" ]
        [ div [ Attributes.class "labelled-icon" ]
            [ Icons.calendar |> Icons.toHtml [ Events.onClick (RouteChanged HabitsRoute) ]
            , text "Home"
            ]
        ]
    ]


renderRegisterRoute : Model -> List (Html Msg)
renderRegisterRoute model =
    [ div []
        [ h1 [] [ text "Sign up!" ]
        , form [ Events.onSubmit (Register model.registerName model.email model.loginPassword) ]
            [ input [ Attributes.placeholder "Name", Events.onInput ChangeRegisterName ] [ text model.registerName ]
            , input [ Attributes.placeholder "Email Address", Events.onInput ChangeEmail ] [ text model.email ]
            , input [ Attributes.placeholder "Password", Attributes.type_ "password", Events.onInput ChangePassword ] [ text model.loginPassword ]
            , button [ Attributes.type_ "submit" ] [ text "Go!" ]
            ]
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
