# Speak & Spell in Elm

My very first attempt at an [Elm](https://elm-lang.org/) application, to test
my understanding of [Elm in
Action](https://www.manning.com/books/elm-in-action).

## CHANGE OF API UPDATE (20240617)

The Random Word API I used to use for this app is justifiably returning an
error and encourages users to deploy their own, most likely because the many
requests would cost them money. I cannot justify deploying my own or forcing
anyone to do so for this mock app. Thus, I have switched to a different free
random word API and refactored the Elm code to adapt to it. The API lacks
definition and pronunciation but at least my Speak and Spell app is available
and usable.

You can find the old/original version with more features [on this branch](https://github.com/gacallea/elm_speakandspell/tree/old_api_version_with_translations).

## Why Speak & Spell?

Because there was potential for a good mix of UI and UX. It seemed like a great
starting point to learn and practice. Moreover, I fancied creating a project
completely from scratch, as opposed to pre-existing concepts.

## Is it a clone?

Being a _toy project_ (no pun intended), this is a limited reproduction:

1. match the word on the screen
2. use the commands

No _Mystery Word_ or any other play mode from the original game.

The point of this exercise was to: study, internalize, apply, learn some more,
improve skills and code, rinse and repeat. Not to be an 1:1 clone.

## Some Background

When I first started this project, I hadn't finished the book yet. In fact, I
started creating this project right after finishing Chapter 5: a chapter that
covered all the basics up to testing. I needed to internalize the concepts I
had learned up to that point, and to make sure I was getting them right.

After improving my skills on
[Exercism](https://exercism.org/profiles/gacallea), and doing some more
exploring, I have:

- migrated the project to [my
  template](https://github.com/gacallea/elm_vite_tailwind_template)
- migrated from [Elm
  UI](https://github.com/gacallea/elm_speakandspell/tree/elm_ui_version) to
  [Tailwind CSS](https://tailwindcss.com/)
- made it fully reponsive to the best of my abilities
- resumed the book from where I left off: Chapter 6 - Testing
- wrote [all
  tests](https://github.com/gacallea/elm_speakandspell/blob/main/tests/SpeakAndSpellTest.elm)
  for the API and more relevant UI items

With all testing complete, the project fully served its purpose and it is now
complete. 🎉 🎉 🎉

## Made With

Tooling privided by my own [Vite, Elm, and Tailwind CSS,
Template](https://github.com/gacallea/elm_vite_tailwind_template). Check it out
😃

## Credits & Copyright

[Speak & Spell](<https://en.wikipedia.org/wiki/Speak_%26_Spell_(toy)>)™ is © of
Texas Instruments Inc.

The favicon used on the Vercel app is © [Gregor
Cresnar](https://thenounproject.com/icon/speak-1616157/). Licensed under the
[Creative Commons CC-BY 3.0](https://creativecommons.org/licenses/by/3.0/).
