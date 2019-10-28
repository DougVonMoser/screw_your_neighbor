port module LocalStorage exposing (..)


port checkForLocalName : () -> Cmd msg


port updateNameInStorage : String -> Cmd msg


port fromLocalStorage : (String -> a) -> Sub a


test =
    "Hi"
