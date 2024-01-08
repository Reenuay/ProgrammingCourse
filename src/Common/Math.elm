module Common.Math exposing (..)


lerp : number -> number -> number -> number
lerp a b t =
    a + (b - a) * t
