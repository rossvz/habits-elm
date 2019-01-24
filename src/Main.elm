port module Main exposing (Habit, Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, object)
import Url



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
    , error : Maybe String
    , showNewHabit : Bool
    , loading : Bool
    }


type alias Entry =
    { id : Int
    , habit : Habit
    , created : String
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
      }
    , case flags.userId of
        Just id ->
            getUserById flags.apiUrl id

        Nothing ->
            Cmd.none
    )


type alias Habit =
    { id : Int
    , name : String
    , category : String
    , weight : Int
    }


type alias User =
    { id : Int
    , name : String
    , email : String
    , habits : List Habit
    }


type UserList
    = UserList (List User)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeEmail email ->
            ( { model | email = email }, Cmd.none )

        AddHabit ->
            let
                newHabit =
                    { name = model.newHabitName, category = "Habit", weight = 1, id = List.length model.habits + 1 }
            in
            ( { model | habits = newHabit :: model.habits, newHabitName = "" }, Cmd.none )

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
                                [ getUserHabitEntriesToday user.id
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
                        [ getUserHabitEntriesToday user.id
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
                removeHabitEntry habit model.entries

              else
                createHabitEntry habit
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


getUserHabitEntriesToday : Int -> Cmd Msg
getUserHabitEntriesToday userId =
    Http.get
        { url = "http://localhost:8383/entries/today?userId=" ++ String.fromInt userId
        , expect = Http.expectJson GotEntries entriesDecoder
        }


createHabitEntry : Habit -> Cmd Msg
createHabitEntry habit =
    Http.post
        { url = "http://localhost:8383/entries"
        , body = Http.jsonBody (entryEncoder habit)
        , expect = Http.expectJson GotHabitEntryCreated entryDecoder
        }


removeHabitEntry : Habit -> List Entry -> Cmd Msg
removeHabitEntry habit entries =
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
                , url = "http://localhost:8383/entries/" ++ String.fromInt habitEntry.id
                , expect = Http.expectJson DeletedEntry entryDecoder
                , headers = []
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                }


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


entryDecoder : Decoder Entry
entryDecoder =
    Decode.succeed Entry
        |> required "id" int
        |> required "habit" habitDecoder
        |> required "created" string


entryEncoder : Habit -> Encode.Value
entryEncoder habit =
    Encode.object [ ( "habit", Encode.int habit.id ) ]



-- ========================================================================== --
--                                    VIEW                                    --
-- ========================================================================== --


view : Model -> Browser.Document Msg
view model =
    { title = "Habits"
    , body =
        [ div [ Attributes.class "container" ]
            [ renderError model.error
            , if model.loading then
                div [] [ text "Loading..." ]

              else
                div [ Attributes.class "app" ]
                    [ case model.user of
                        Nothing ->
                            form [ Events.onSubmit (Login model.email) ]
                                [ p [] [ text "Enter your email address to log in:" ]
                                , input [ Attributes.placeholder "Email address", Attributes.class "input", Events.onInput ChangeEmail, Attributes.value model.email ] []
                                , div [ Attributes.class "separator" ] []
                                , button [ Attributes.type_ "submit" ] [ text "Go" ]
                                ]

                        Just _ ->
                            div []
                                [ h1 [ Attributes.class "user" ]
                                    [ text
                                        "Habits"
                                    ]
                                , h2 []
                                    [ text ((model.entries |> List.length |> String.fromInt) ++ " point(s) earned today")
                                    ]
                                , renderHabits model.habits model.entries
                                , if model.showNewHabit then
                                    newHabitForm model

                                  else
                                    div [ Attributes.class "footer" ]
                                        [ span [ Events.onClick Logout ] [ text "Logout" ]
                                        , button [ Events.onClick ToggleShowNewHabit ] [ text "+" ]
                                        ]
                                ]
                    ]
            ]
        ]
    }


renderError : Maybe String -> Html Msg
renderError maybeError =
    case maybeError of
        Nothing ->
            span [] []

        Just error ->
            div [ Attributes.class "error-container" ] [ text error ]


renderHabits : List Habit -> List Entry -> Html Msg
renderHabits habits entries =
    div [ Attributes.class "habits-container" ] (habits |> List.map (renderHabit entries))


renderHabit : List Entry -> Habit -> Html Msg
renderHabit entries habit =
    div
        [ Attributes.class
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
    form [ Events.onSubmit AddHabit ]
        [ input [ Attributes.placeholder "Habit", Attributes.value model.newHabitName, Events.onInput HandleNewHabitChange ] []
        , input [ Attributes.placeholder "Category", Attributes.value model.newHabitCategory, Events.onInput HandleNewHabitCategoryChange ] []
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
