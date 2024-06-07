import '../css/style.css'
import { Elm } from '../elm/SpeakAndSpell.elm'

// Start the Elm application.
const app = Elm.SpeakAndSpell.init({
  node: document.querySelector('main')
})

// Instantiate Speech Synth API
const synth = window.speechSynthesis

// https://github.com/gacallea/elm_speakandspell/pull/2
function speak (message) {
  const utter = new window.SpeechSynthesisUtterance()
  utter.text = message
  synth.speak(utter)
}

// Pause/Resume Speech Synth API (SetSound On | Off)
app.ports.sound.subscribe((message) => {
  synth.cancel()
  if (message === true) {
    synth.resume()
  } else {
    synth.pause()
  }
})

// We receive the whole word here and speak it
app.ports.speak.subscribe((message) => {
  speak(message)
})

// We receive the split word here and spell it
app.ports.spell.subscribe((message) => {
  speak(message)
})
