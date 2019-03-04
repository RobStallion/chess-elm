module Main exposing (main)

import Browser
import State exposing (init, update)
import View exposing (view)


main =
    Browser.sandbox { init = init, update = update, view = view }
