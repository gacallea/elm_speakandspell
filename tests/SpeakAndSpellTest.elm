module SpeakAndSpellTest exposing
    ( clickCommands
    , clickLetters
    , clickSoundControls
    , getWordsFromAPI
    , isInitializedOutputScreen
    , isPresentAppColours
    , isPresentAppName
    , isPresentBrandLink
    , isPresentBrandLogo
    , isPresentBrandName
    , isPresentCommands
    , isPresentKeyboard
    , isPresentLoading
    , isPresentShellLogo
    , isPresentSoundControls
    )

import Expect
import Fuzz exposing (string)
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import SpeakAndSpell
    exposing
        ( Model
        , Msg(..)
        , Sound(..)
        , elmLogoBlue
        , elmLogoGray
        , initialModel
        , namePlusLogo
        , namePlusSoundCtrl
        , newWordDecoder
        , outputScreen
        , outputText
        , theKeyboard
        , viewLoading
        )
import Test exposing (Test, describe, fuzz3, test)
import Test.Html.Event as Event
import Test.Html.Query as Query exposing (Single)
import Test.Html.Selector exposing (attribute, classes, tag, text)



-- CONSTANTS


speakAndSpell : List ( String, String )
speakAndSpell =
    [ ( "Speak", "text-red-600" )
    , ( "&", "text-white" )
    , ( "Spell", "text-blue-600" )
    ]


loadingText : List String
loadingText =
    [ "L", "O", "A", "D", "I", "N", "G" ]


alphabet : List String
alphabet =
    -- A to Z in ASCII is 65 to 90
    List.range 65 90
        |> List.map (\ascii -> String.fromChar (Char.fromCode ascii))


keyboardCommands : List ( Msg, String )
keyboardCommands =
    [ ( EraseLetter, "ERASE [↤]" )
    , ( ResetWord, "RESET [5]" )
    , ( Speak, "SPEAK [8]" )
    , ( Spell, "SPELL [9]" )
    , ( SubmitWord, "SUBMIT [↵]" )
    , ( ResetWord, "RETRY [6]" )
    , ( GetAnotherWord, "NEW [0]" )
    ]


soundCommands : List ( Msg, String )
soundCommands =
    [ ( SetSound Off, "SOUND OFF [3]" )
    , ( SetSound On, "SOUND ON [2]" )
    ]



-- HELPER FUNCTIONS


findAriaLabel : Html msg -> String -> String -> Single msg
findAriaLabel componentToTest ariaLabelCommon ariaLabelSpecific =
    componentToTest
        |> Query.fromHtml
        |> Query.find
            [ attribute
                (Attr.attribute "aria-label"
                    (ariaLabelCommon ++ ariaLabelSpecific)
                )
            ]


allThingsClicker : Html Msg -> Msg -> String -> String -> Test
allThingsClicker componentToTest message ariaToFind item =
    test ("clicking " ++ item) <|
        \_ ->
            findAriaLabel componentToTest ariaToFind (" " ++ item)
                |> Event.simulate Event.click
                |> Event.expect message


allThingsChecker : Html msg -> String -> String -> String -> Test
allThingsChecker componentToTest ariaToFind tagToFind item =
    test ("is present " ++ item) <|
        \_ ->
            findAriaLabel componentToTest ariaToFind (" " ++ item)
                |> Query.has [ tag tagToFind, text item ]


outputScreenQueryHtml : Single Msg
outputScreenQueryHtml =
    initialModel
        |> outputScreen
        |> Query.fromHtml



-- LOADING SCREEN TESTS


isPresentLoading : Test
isPresentLoading =
    describe "all letters are present on loading screen" <|
        List.map (\letter -> checkLoadingLetters letter) loadingText


checkLoadingLetters : String -> Test
checkLoadingLetters letter =
    test ("loading letter present " ++ letter) <|
        \_ ->
            findAriaLabel viewLoading "Loading Animation" ""
                |> Query.has [ tag "p", text letter ]



-- BRAND, APP NAME, AND LOGOS TESTS


isPresentBrandName : Test
isPresentBrandName =
    test "brand name present" <|
        \_ ->
            outputScreenQueryHtml
                |> Query.has [ tag "a", text "Elm Instruments" ]


isPresentBrandLink : Test
isPresentBrandLink =
    test "brand link present" <|
        \_ ->
            outputScreenQueryHtml
                |> Query.has [ tag "a", attribute (Attr.href "https://elm-lang.org/") ]


isPresentBrandLogo : Test
isPresentBrandLogo =
    test "brand logo present" <|
        \_ ->
            outputScreenQueryHtml
                |> Query.has [ tag "img", attribute (Attr.src elmLogoGray) ]


isPresentShellLogo : Test
isPresentShellLogo =
    test "yellow shell logo present" <|
        \_ ->
            findAriaLabel namePlusLogo "Elm Logo" ""
                |> Query.has [ tag "img", attribute (Attr.src elmLogoBlue) ]


isPresentAppName : Test
isPresentAppName =
    describe "app name words are present on yellow shell" <|
        List.map (\word -> checkAppNameWording (Tuple.first word)) speakAndSpell


checkAppNameWording : String -> Test
checkAppNameWording word =
    test ("speak and spell word " ++ word) <|
        \_ ->
            findAriaLabel namePlusLogo "App Name" ""
                |> Query.has [ tag "p", text word ]


isPresentAppColours : Test
isPresentAppColours =
    describe "all app name words have the right colours" <|
        List.map (\colour -> checkAppNameColours (Tuple.second colour)) speakAndSpell


checkAppNameColours : String -> Test
checkAppNameColours colour =
    test ("speak and spell colour " ++ colour) <|
        \_ ->
            findAriaLabel namePlusLogo "App Name" ""
                |> Query.has [ tag "p", classes [ colour ] ]



-- API TESTS


getWordsFromAPI : Test
getWordsFromAPI =
    fuzz3 string string string "get new words from the words API" <|
        \word definition pronunciation ->
            [ ( "word", Encode.string word )
            , ( "definition", Encode.string definition )
            , ( "pronunciation", Encode.string pronunciation )
            ]
                |> Encode.object
                |> decodeValue newWordDecoder
                |> Expect.ok



-- OUTPUT SCREEN TESTS


isInitializedOutputScreen : Model -> Test
isInitializedOutputScreen model =
    test "render the output screen with the default message" <|
        \_ ->
            outputText model
                |> Expect.equal "START TYPING TO MATCH THE WORD ABOVE"



-- CLICKING TESTS


clickLetters : Model -> Test
clickLetters model =
    describe "click all letters keys on the onscreen keyboard" <|
        List.map
            (\letter ->
                allThingsClicker (theKeyboard model) (KeyClicked letter) "Keyboard Key" letter
            )
            alphabet


clickCommands : Model -> Test
clickCommands model =
    describe "click all onscreen keyboard commands" <|
        List.map
            (\cmd ->
                allThingsClicker (theKeyboard model) (Tuple.first cmd) "Command" (Tuple.second cmd)
            )
            keyboardCommands


clickSoundControls : Model -> Test
clickSoundControls model =
    describe "click all sound controls commands" <|
        List.map
            (\cmd ->
                allThingsClicker (namePlusSoundCtrl model) (Tuple.first cmd) "Command" (Tuple.second cmd)
            )
            soundCommands



-- LETTERS TESTS


isPresentKeyboard : Model -> Test
isPresentKeyboard model =
    describe "all letters are present on the onscreen keyboard" <|
        List.map
            (\letter ->
                allThingsChecker (theKeyboard model) "Keyboard Key" "button" letter
            )
            alphabet



-- COMMANDS TESTS


isPresentCommands : Model -> Test
isPresentCommands model =
    describe "all commands are present on the onscreen keyboard" <|
        List.map
            (\command ->
                allThingsChecker (theKeyboard model) "Command" "button" (Tuple.second command)
            )
            keyboardCommands


isPresentSoundControls : Model -> Test
isPresentSoundControls model =
    describe "all sound controls are present" <|
        List.map
            (\command ->
                allThingsChecker (namePlusSoundCtrl model) "Command" "button" (Tuple.second command)
            )
            soundCommands
