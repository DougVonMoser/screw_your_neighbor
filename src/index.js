'use strict';

require("./styles.scss");
import "./assets/svg-cards.svg"


import { bind } from "./webSocket.js";

const {Elm} = require('./Main');
var app = Elm.Main.init({
    flags: 6,
    node: document.getElementById("elm-node")
});

// websocket binding
bind(app)

// localstorage stuff
app.ports.checkForLocalName.subscribe(() => {
    let myName = sessionStorage.getItem('myName')
    console.log('checked sessionStorage for myName, found:', myName)
    if(myName) {
        app.ports.fromLocalStorage.send(myName)
    }
})

app.ports.updateNameInStorage.subscribe((name) => {
    sessionStorage.setItem('myName', name)
    console.log('updated session storage myName with:', name)
})
