port module SpeakAndSpell exposing
    ( Model
    , Msg(..)
    , NewWord
    , Output(..)
    , Sound(..)
    , Status(..)
    , elmLogoBlue
    , elmLogoGray
    , initialModel
    , main
    , namePlusLogo
    , outputScreen
    , rootDecoder
    , viewLoading
    )

import Accessibility.Aria as Aria
import Browser
import Browser.Events
import Html exposing (Html, a, button, div, img, main_, p, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import VitePluginHelper exposing (asset)



-- CONSTANTS


randomWordsApiUrl : String
randomWordsApiUrl =
    "https://random-word.ryanrk.com//api/en/word/random"


elmLogoBlue : String
elmLogoBlue =
    asset "../img/ElmLogoBlue.svg"


elmLogoGray : String
elmLogoGray =
    asset "../img/ElmLogoGray.svg"



-- MESSAGES


type Msg
    = GetNewWord (Result Http.Error (List NewWord))
    | KeyPressed KeyboardEvent
    | KeyClicked String
    | GetAnotherWord
    | EraseLetter
    | ResetWord
    | SubmitWord
    | SetSound Sound
    | Speak
    | Spell



-- TYPES


type Status
    = Loading
    | Loaded NewWord
    | Errored Http.Error


type Output
    = Init
    | Word
    | Result


type Sound
    = On
    | Off


type alias NewWord =
    String



-- MODEL


type alias Model =
    { status : Status
    , output : Output
    , sound : Sound
    , newWord : NewWord
    , guessWord : String
    , checkWord : String
    , result : String
    }



-- INIT


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { status = Loading
      , output = Init
      , sound = On
      , newWord = "INIT"
      , guessWord = ""
      , checkWord = ""
      , result = ""
      }
    , getNewWordCmd
    )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- COMMANDS


getNewWordCmd : Cmd Msg
getNewWordCmd =
    Http.get
        { url = randomWordsApiUrl
        , expect = Http.expectJson GetNewWord rootDecoder
        }


rootDecoder : Json.Decode.Decoder (List String)
rootDecoder =
    Json.Decode.list Json.Decode.string



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (Json.Decode.map KeyPressed decodeKeyboardEvent)



-- PORTS


port speak : String -> Cmd msg


port spell : List String -> Cmd msg


port sound : Bool -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    main_
        [ Aria.label "Main Content"
        , Attr.class "font-mono font-medium container m-auto lg:my-2 max-w-[54rem]"
        ]
        [ case model.status of
            Loading ->
                viewLoading

            Loaded word ->
                viewLoaded word model

            Errored errorMessage ->
                viewErrored errorMessage
        ]


viewLoading : Html msg
viewLoading =
    let
        loadingText : List ( String, String )
        loadingText =
            [ ( "L", "animate-bounce-up" )
            , ( "O", "animate-bounce-down" )
            , ( "A", "animate-bounce-up" )
            , ( "D", "animate-bounce-down" )
            , ( "I", "animate-bounce-up" )
            , ( "N", "animate-bounce-down" )
            , ( "G", "animate-bounce-up" )
            , ( ".", "animate-wiggle" )
            , ( ".", "animate-wiggle" )
            , ( ".", "animate-wiggle" )
            ]
    in
    yellowShell namePlusLogo <|
        div
            -- blue around animation
            [ Aria.label "Loading Screen"
            , Attr.class "bg-sky-500 border border-black rounded-2xl m-2 px-3 py-12"
            ]
            [ div
                [ Aria.label "Loading Animation"
                , Attr.class "flex flex-nowrap"
                ]
              <|
                List.map
                    (\loadText ->
                        loadingLetter (Tuple.first loadText) (Tuple.second loadText)
                    )
                    loadingText
            ]


viewErrored : Http.Error -> Html msg
viewErrored errorMessage =
    yellowShell namePlusLogo <|
        div
            -- red around error message
            [ Aria.label "Error Screen"
            , Attr.class "bg-red-500 border border-black rounded-2xl m-2 px-3 py-12"
            ]
            [ p
                [ Aria.label "Error Message"
                , Attr.class "text-white text-sm md:text-lg lg:text-xl text-center"
                ]
                [ text ("Error: " ++ errorToString errorMessage) ]
            ]


viewLoaded : NewWord -> Model -> Html Msg
viewLoaded newWord model =
    div
        -- outer shell
        [ Aria.label "Loaded App"
        , Attr.class "bg-shell_orange border md:rounded-t-[2.5rem] md:rounded-b-[5rem]"
        ]
        [ div [] [ newWordScreen newWord ]
        , div [] [ outputScreen model ]
        , div [] [ yellowShell namePlusSoundCtrl theKeyboard ]
        ]


yellowShell : Html msg -> Html msg -> Html msg
yellowShell rightContent leftContent =
    div
        -- yellow shell
        [ Attr.class "bg-yellow-300 m-4 md:mx-8 md:mt-10 md:mb-36 px-1 md:px-4 py-3 md:py-8 rounded-t-xl md:rounded-t-2xl rounded-b-3xl md:rounded-b-[3rem]" ]
        [ leftContent
        , rightContent
        ]


speakAndSpellName : Html msg
speakAndSpellName =
    div
        [ Aria.label "Brand Name"
        , Attr.class "font-serif text-3xl md:text-4xl lg:text-5xl font-bold flex"
        ]
        [ p
            [ Attr.class "text-red-600 pr-1" ]
            [ text "Speak" ]
        , p
            [ Attr.class "text-white pr-1" ]
            [ text "&" ]
        , p
            [ Attr.class "text-blue-600" ]
            [ text "Spell" ]
        ]


namePlusLogo : Html msg
namePlusLogo =
    div
        [ Aria.label "App name and Elm logo"
        , Attr.class "flex justify-between my-12 mx-2"
        ]
        [ div
            [ Attr.class "my-auto", Aria.label "App Name" ]
            [ speakAndSpellName ]
        , div
            [ Attr.class "my-auto", Aria.label "Elm Logo" ]
            [ img [ Attr.src elmLogoBlue, Attr.class "w-10 h-10 md:w-20 md:h-20 lg:w-24 lg:h-24" ] [] ]
        ]


namePlusSoundCtrl : Html Msg
namePlusSoundCtrl =
    let
        soundCommands : List { cmdMsg : Msg, cmdName : String }
        soundCommands =
            [ { cmdMsg = SetSound Off, cmdName = "SOUND OFF [3]" }
            , { cmdMsg = SetSound On, cmdName = "SOUND ON [2]" }
            ]
    in
    div
        [ Attr.class "flex flex-col md:flex-row my-8 md:my-12 lg:my-14" ]
        [ div
            [ Attr.class "grow self-center" ]
            [ speakAndSpellName ]
        , div
            -- sound controls
            [ Aria.label "Sound Commands"
            , Attr.class "my-auto self-center"
            ]
          <|
            List.map
                (\cmd ->
                    blueCommandBtn cmd.cmdMsg cmd.cmdName
                )
                soundCommands
        ]


theKeyboard : Html Msg
theKeyboard =
    let
        keyboardCommands : List { cmdMsg : Msg, cmdName : String }
        keyboardCommands =
            [ { cmdMsg = EraseLetter, cmdName = "ERASE [↤]" }
            , { cmdMsg = ResetWord, cmdName = "RESET [5]" }
            , { cmdMsg = Speak, cmdName = "SPEAK [8]" }
            , { cmdMsg = Spell, cmdName = "SPELL [9]" }
            , { cmdMsg = SubmitWord, cmdName = "SUBMIT [↵]" }
            , { cmdMsg = ResetWord, cmdName = "RETRY [6]" }
            , { cmdMsg = GetAnotherWord, cmdName = "NEW [0]" }
            ]
    in
    div
        -- blue around keyboard
        [ Attr.class "bg-sky-500 flex flex-col border border-black rounded-2xl m-1 md:m-0 px-1 lg:px-4 py-4 md:py-8" ]
        [ div
            -- keyboard letters
            [ Aria.label "Keyboard from A to Z"
            , Attr.class "flex flex-wrap"
            ]
          <|
            alphabetRow 65 90
        , div
            -- keyboard commands
            [ Aria.label "Keyboard Commands"
            , Attr.class "flex flex-wrap"
            ]
          <|
            List.map
                (\cmd ->
                    yellowCommandBtn cmd.cmdMsg cmd.cmdName
                )
                keyboardCommands
        ]


newWordScreen : NewWord -> Html Msg
newWordScreen newWord =
    div
        -- new word "top screen"
        [ Aria.label "New Word Screen"
        , Attr.class "bg-sky-500 text-base md:text-lg lg:text-xl flex flex-col justify-between m-2 md:mb-8 md:mt-8 md:mx-6 rounded-lg md:rounded-3xl border border-solid border-black"
        ]
        [ div [ Attr.class "px-4 py-4 pb-2 md:px-8 md:py-8" ]
            [ p
                [ Aria.label "New Word"
                ]
                [ text ("Your word is: " ++ String.toUpper newWord) ]
            , p
                [ Aria.label "Word Definition"
                ]
                [ text "Definition: N/A with this API" ]
            , p
                [ Aria.label "Word Pronunciation"
                ]
                [ text "Pronunciation: N/A with this API" ]
            ]
        ]


outputScreen : Model -> Html msg
outputScreen model =
    div
        -- output screen
        [ Aria.label "Output Screen"
        , Attr.class "bg-black h-32 md:h-44 lg:h-48 flex flex-col justify-between"
        ]
        [ div
            [ Attr.class "text-center" ]
            [ p
                [ Aria.label "Output Text"
                , Attr.class "font-lcd text-lcd_text px-4 text-base md:text-2xl pt-8 md:pt-16"
                ]
                [ text (outputText model) ]
            ]
        , div
            [ Aria.label "Elm Branding"
            , Attr.class "inline-flex self-end mr-4 md:mr-12 mb-2"
            ]
            [ img
                [ Attr.src elmLogoGray
                , Attr.alt "Elm Logo"
                , Attr.title "Elm Logo"
                , Attr.class "w-2 h-2 md:w-3 md:h-3 lg:w-4 lg:h-4 self-center"
                ]
                []
            , a
                [ Attr.href "https://elm-lang.org/"
                , Attr.target "_blank"
                , Attr.rel "noreferrer noopener"
                , Attr.class "text-stone-400 text-xs md:text-base lg:text-lg pl-2"
                ]
                [ text "Elm Instruments" ]
            ]
        ]


loadingLetter : String -> String -> Html msg
loadingLetter labelText animation =
    p
        [ Attr.class "grow basis-auto text-center m-[0.025rem] md:m-1 py-1 md:py-2 px-1 md:px-4 bg-amber-400 md:text-2xl lg:text-4xl font-semibold border-2 md:border-4 border-orange-600 rounded-md"
        , Attr.class animation
        ]
        [ text labelText ]


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " is invalid"

        Http.Timeout ->
            "Unable to reach the server, try again later"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage


commandBtn : String -> Msg -> String -> Html Msg
commandBtn bgColor pressAction labelText =
    button
        [ Aria.label ("Command " ++ labelText)
        , Attr.class "text-[0.5rem] md:text-xs lg:text-sm md:font-semibold grow basis-3 md:basis-auto mt-1 mx-1 p-2 hover:bg-amber-700 hover:text-white rounded-md md:rounded-xl border-solid border border-black"
        , Attr.class bgColor
        , onClick pressAction
        ]
        [ text labelText ]


yellowCommandBtn : Msg -> String -> Html Msg
yellowCommandBtn pressAction labelText =
    commandBtn "bg-amber-400" pressAction labelText


blueCommandBtn : Msg -> String -> Html Msg
blueCommandBtn pressAction labelText =
    commandBtn "bg-sky-500" pressAction labelText


outputText : Model -> String
outputText model =
    case model.output of
        Init ->
            "START TYPING TO MATCH THE WORD ABOVE"

        Word ->
            model.guessWord

        Result ->
            model.result


alphabetRow : Int -> Int -> List (Html Msg)
alphabetRow start end =
    List.range start end
        |> List.map
            (\asciiCode ->
                button
                    [ Aria.label ("Keyboard Key " ++ codeToString asciiCode)
                    , Attr.class "text-xs md:text-sm md:font-semibold grow basis-6 md:basis-11 lg:basis-12 m-1 p-1 lg:px-4 md:py-4 border border-black rounded-md md:rounded-lg bg-orange-500 hover:bg-amber-700 hover:text-white"
                    , onClick (KeyClicked (codeToString asciiCode))
                    ]
                    [ text (codeToString asciiCode)
                    ]
            )


codeToString : Int -> String
codeToString asciiCode =
    String.fromChar (Char.fromCode asciiCode)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNewWord (Ok word) ->
            case word of
                _ :: _ ->
                    ( { model
                        | status = Loaded (unwrapNewWordList word)
                        , checkWord = setCheckWord (unwrapNewWordList word)
                      }
                    , Cmd.none
                    )

                [] ->
                    ( { model | status = Errored (Http.BadBody "No words found :(") }
                    , Cmd.none
                    )

        GetNewWord (Err err) ->
            ( { model | status = Errored err }
            , Cmd.none
            )

        KeyPressed event ->
            kbdEventToCommand event model

        KeyClicked string ->
            ( appendToGuessWord model string
            , speak (String.toLower string)
            )

        GetAnotherWord ->
            ( resetWord model
            , getNewWordCmd
            )

        EraseLetter ->
            ( if isGuessEmpty (eraseLetter model) then
                resetWord model

              else
                eraseLetter model
            , Cmd.none
            )

        ResetWord ->
            ( resetWord model
            , Cmd.none
            )

        SubmitWord ->
            ( submitWord model
            , speak (checkResult model)
            )

        SetSound param ->
            ( model
            , setSound param
            )

        Speak ->
            ( wordToScreen model
            , speak (wordToSpeak model)
            )

        Spell ->
            ( wordToScreen model
            , spell (splitToSpell (wordToSpeak model))
            )



-- UPDATE HELPERS


kbdEventToCommand : KeyboardEvent -> Model -> ( Model, Cmd Msg )
kbdEventToCommand event model =
    if
        event.altKey
            || event.ctrlKey
            || event.metaKey
            || event.repeat
            || event.shiftKey
    then
        ( model, Cmd.none )

    else
        case event.key of
            Just "2" ->
                ( model
                , setSound On
                )

            Just "3" ->
                ( model
                , setSound Off
                )

            Just "5" ->
                ( resetWord model
                , Cmd.none
                )

            Just "6" ->
                ( resetWord model
                , Cmd.none
                )

            Just "8" ->
                ( wordToScreen model
                , speak (wordToSpeak model)
                )

            Just "9" ->
                ( wordToScreen model
                , spell (splitToSpell (wordToSpeak model))
                )

            Just "0" ->
                ( resetWord model
                , getNewWordCmd
                )

            Just "Backspace" ->
                ( if isGuessEmpty (eraseLetter model) then
                    resetWord model

                  else
                    eraseLetter model
                , Cmd.none
                )

            Just "Enter" ->
                ( submitWord model
                , speak (checkResult model)
                )

            _ ->
                ( if isStringEmpty (kbdEventToString event) then
                    model

                  else
                    appendToGuessWord model (kbdEventToString event)
                , speak (String.toLower (kbdEventToString event))
                )


isStringEmpty : String -> Bool
isStringEmpty string =
    String.isEmpty string


isGuessEmpty : Model -> Bool
isGuessEmpty model =
    String.isEmpty model.guessWord


appendToGuessWord : Model -> String -> Model
appendToGuessWord model string =
    { model | guessWord = String.append model.guessWord string, output = Word }


resetWord : Model -> Model
resetWord model =
    { model | guessWord = "", result = "", output = Init }


eraseLetter : Model -> Model
eraseLetter model =
    { model | guessWord = String.dropRight 1 model.guessWord, result = "", output = Word }


submitWord : Model -> Model
submitWord model =
    { model | result = checkResult model, output = Result }


checkResult : Model -> String
checkResult model =
    if isGuessEmpty model then
        "AN EMPTY STRING IS NEVER THE ANSWER..."

    else if model.guessWord == model.checkWord then
        "CONGRATULATIONS! " ++ model.guessWord ++ " IS CORRECT!"

    else
        "OH NO... " ++ model.guessWord ++ " ISN'T RIGHT..."


setSound : Sound -> Cmd Msg
setSound switch =
    case switch of
        On ->
            sound True

        Off ->
            sound False


wordToScreen : Model -> Model
wordToScreen model =
    if isGuessEmpty model then
        { model | output = Init }

    else
        { model | output = Word }


splitToSpell : String -> List String
splitToSpell word =
    String.split "" word


wordToSpeak : Model -> String
wordToSpeak model =
    String.toLower model.guessWord


kbdEventToString : KeyboardEvent -> String
kbdEventToString event =
    case event.key of
        Just key ->
            if
                String.all Char.isAlpha key
                    && String.length key
                    == 1
            then
                String.toUpper key

            else
                ""

        Nothing ->
            ""


unwrapNewWordList : List NewWord -> NewWord
unwrapNewWordList wordsList =
    case List.head wordsList of
        Just word ->
            word

        Nothing ->
            "NOTHING"


setCheckWord : NewWord -> String
setCheckWord word =
    String.toUpper word
