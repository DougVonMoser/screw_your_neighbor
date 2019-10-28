'use strict';

require("./styles.scss");
import "./assets/svg-cards.svg"


import { bind } from "./webSocket.js";

const {Elm} = require('./Main');
var app = Elm.Main.init({
    flags: 6,
    node: document.getElementById("elm-node")
});

bind(app)

