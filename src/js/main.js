import '../sass/main.sass'
import 'bootstrap/dist/css/bootstrap.min.css'
import 'bootstrap-icons/font/bootstrap-icons.css'
import { Elm } from '../elm/Main.elm'

const settingsJson = window.localStorage.getItem('settings')
let flags
if (settingsJson !== null) {
  flags = settingsJson
} else {
  flags = JSON.stringify({
    usage_rate_map: [
      { usage: 50, rate: 3.75 },
      { usage: 75, rate: 4.19 },
      { usage: 124, rate: 5.72 },
      { usage: 99, rate: 6.00 },
      { usage: 99, rate: 6.34 },
      { usage: 199, rate: 9.94 },
      { usage: -1, rate: 11.46 }
    ],
    vat_percentage: 5,
    demand_charge: 70
  })
}

const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: flags
})

app.ports.saveSettings.subscribe(function (json) {
  window.localStorage.setItem('settings', json)
})
