const {Elm} = require('./Main.elm');

Elm.Main.init({
    node: document.getElementById('elm'),
    flags: Math.floor(Math.random()*0x0FFFFFFF)
});
