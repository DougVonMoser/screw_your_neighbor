module Cards exposing (..)

import Array
import Html
import Random


type alias Card =
    { suit : String
    , number : Int
    , svgDisplayVal : String
    }


type alias Model =
    Array.Array Card


init : ( Model, Cmd Msg )
init =
    ( Array.empty, Random.generate HeresASeed (Random.int Random.minInt Random.maxInt) )


removeAt : Int -> Array.Array a -> Array.Array a
removeAt idx source =
    if idx == (Array.length source - 1) then
        Array.slice 0 idx source

    else
        Array.fromList
            (List.concat
                [ Array.toList (Array.slice 0 idx source)
                , Array.toList (Array.slice (idx + 1) (Array.length source) source)
                ]
            )


shuffleHelper : Random.Seed -> Array.Array a -> Array.Array a -> Array.Array a
shuffleHelper seed source result =
    if Array.isEmpty source then
        result

    else
        let
            indexGenerator =
                Random.int 0 (Array.length source - 1)

            ( index, nextSeed ) =
                Random.step indexGenerator seed

            valAtIndex =
                case Array.get index source of
                    Just value ->
                        value

                    Nothing ->
                        Debug.todo "oops a poopers"

            sourceWithoutIndex =
                removeAt index source
        in
        shuffleHelper nextSeed sourceWithoutIndex (Array.push valAtIndex result)


shuffleDem : Random.Seed -> Array.Array a -> Array.Array a
shuffleDem seed original =
    shuffleHelper seed original Array.empty


type Msg
    = HeresASeed Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HeresASeed randomInt ->
            ( getFullShuffledDeck randomInt, Cmd.none )


getFullShuffledDeck : Int -> Model
getFullShuffledDeck randomInt =
    getFittyTwo |> shuffleDem (Random.initialSeed randomInt)


getFittyTwo : Array.Array Card
getFittyTwo =
    let
        suits =
            [ "heart", "spade", "diamond", "club" ]

        numbers =
            List.range 1 13

        displayValues =
            List.append (List.map String.fromInt (List.range 1 11)) [ "jack", "queen", "king" ]
    in
    List.map
        (\x ->
            List.map2
                (\num val ->
                    { suit = x
                    , number = num
                    , svgDisplayVal = x ++ "_" ++ val
                    }
                )
                numbers
                displayValues
        )
        suits
        |> List.concat
        |> Array.fromList


view : Model -> Html.Html Msg
view model =
    Html.ul []
        (Array.toList
            (Array.map
                (\x ->
                    Html.li []
                        [ Html.text (Debug.toString x) ]
                )
                model
            )
        )
