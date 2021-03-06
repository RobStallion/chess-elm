# chess-elm

Creating a chess game with elm.

<img src="https://user-images.githubusercontent.com/15571853/54646448-eb047b00-4a96-11e9-83fe-6fb604cd6e09.gif" width=600px />

## Run Locally

Start by cloning the repo...
```
$ git clone git@github.com:RobStallion/chess-elm.git && cd chess-elm
```

### Dependencies

Install elm. Installation instructions can be found [here](https://guide.elm-lang.org/install.html)

### Run with elm reactor

To get to project up and running just do the following steps...
```
$ elm make src/Main.elm --output public/js/elm.js
$ elm reactor
Visit localhost:8000 in your browser and navigate to public/index.html
```

And that's it 🎉🎉🎉🎉🎉

### Run in dev mode

If you want to run this application in a dev environment I would recommend
installing [elm-live](https://github.com/wking-io/elm-live). Installation
instructions can be found
[here](https://github.com/wking-io/elm-live#installation)

Once you have installed `elm-live`, do the following...
```
$ elm-live src/Main.elm --open --dir=public -- --output=public/js/elm.js --debug
```

## Why

I decided to start building this because I like chess (although, sadly, I am not
very good) and because I really like elm. I thought it would be a good way for
me to sharpen my elm skills while also **HOPEFULLY** gaining a better
understanding of chess.
