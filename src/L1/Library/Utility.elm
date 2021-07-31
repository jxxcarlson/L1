module L1.Library.Utility exposing
    ( characterAt
    , clipEnds
    , commaSeparatedToList
    , debug
    , debug2
    , dropWords
    , entities
    , ifApply
    , keyValueDict
    , liftToMaybe
    , mapTriple
    , normalize
    , quote
    , roundTo
    , squeeze
    , unquote
    )

import Dict exposing (Dict)
import L1.Library.Console as Console
import Maybe.Extra
import Regex


characterAt : Int -> String -> String
characterAt k str =
    String.slice k (k + 1) str


debug str x =
    -- Debug.log (Console.yellow str) x
    x


debug2 str x =
    -- Debug.log (Console.cyan str) x
    x



-- str ++ x


squeeze : String -> String
squeeze str =
    String.replace " " "" str


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


normalize : String -> String
normalize string =
    userReplace " +" (\_ -> " ") string


ifApply : Bool -> (a -> b) -> (a -> b) -> (a -> b)
ifApply condition f g =
    if condition then
        f

    else
        g


clipEnds : String -> String
clipEnds str =
    str |> String.dropLeft 1 |> String.dropRight 1


quote : String -> String
quote str =
    "\"" ++ str ++ "\""


unquote : String -> String
unquote str =
    str |> unquoteLeft |> unquoteRight


unquoteLeft : String -> String
unquoteLeft str =
    if String.left 1 str == "\"" then
        String.dropLeft 1 str

    else
        str


unquoteRight : String -> String
unquoteRight str =
    if String.right 1 str == "\"" then
        String.dropRight 1 str

    else
        str


roundTo : Int -> Float -> Float
roundTo k x =
    let
        factor =
            10.0 ^ toFloat k
    in
    toFloat (round (factor * x)) / factor


commaSeparatedToList : String -> List String
commaSeparatedToList str =
    str |> String.split "," |> List.map String.trim


entities : List String -> List String
entities strings_ =
    List.map (String.split ":") strings_
        |> List.map (List.map String.trim)
        |> List.filter (\x -> List.length x == 1)
        |> List.map List.head
        |> Maybe.Extra.values


keyValueDict : List String -> Dict String String
keyValueDict strings_ =
    List.map (String.split ":") strings_
        |> List.map (List.map String.trim)
        |> List.map pairFromList
        |> Maybe.Extra.values
        |> Dict.fromList


pairFromList : List String -> Maybe ( String, String )
pairFromList strings =
    case strings of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


liftToMaybe : (a -> b) -> (Maybe a -> Maybe b)
liftToMaybe f =
    \a ->
        case a of
            Nothing ->
                Nothing

            Just a_ ->
                Just (f a_)


mapTriple f ( a, b, c ) =
    ( f a, f b, f c )


dropWords : Int -> String -> String
dropWords k str =
    str |> String.words |> List.drop k |> String.join " "
