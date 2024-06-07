module SpeakAndSpellTest exposing
    ( getWordsFromAPI
    , isPresentAppColours
    , isPresentAppName
    , isPresentBrandLink
    , isPresentBrandLogo
    , isPresentBrandName
    , isPresentLoading
    , isPresentShellLogo
    )

import Expect
import Fuzz exposing (string)
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import SpeakAndSpell
    exposing
        ( Msg
        , elmLogoBlue
        , elmLogoGray
        , initialModel
        , namePlusLogo
        , newWordDecoder
        , outputScreen
        , viewLoading
        )
import Test exposing (Test, describe, fuzz3, test)
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
-- CLICKING TESTS
-- LETTERS TESTS
-- COMMANDS TESTS
