module L1.Library.Vector exposing (increment, init, toString)

import List.Extra


type alias Vector =
    { size : Int, content : List Int }


{-|

    > v = init 4
    { content = [0,0,0,0], size = 4 }

-}
init : Int -> Vector
init k =
    { size = k, content = List.repeat k 0 }


toString : Vector -> String
toString v =
    v.content
        |> List.filter (\x -> x > 0)
        |> List.map String.fromInt
        |> String.join "."


get : Int -> Vector -> Int
get k v =
    List.Extra.getAt k v.content |> Maybe.withDefault 0


set : Int -> Int -> Vector -> Vector
set k a v =
    { v | content = List.Extra.setAt k a v.content }


resetFrom : Int -> Vector -> Vector
resetFrom k v =
    let
        prefix =
            List.take k v.content

        suffix =
            List.repeat (v.size - k) 0
    in
    { size = v.size, content = prefix ++ suffix }


{-|

    # increment section, then subsection
    > v |> increment 0 |> increment 1
    { content = [1,1,0,0], size = 4 }

    # increment section, then subsection, then section
    > v |> increment 0 |> increment 1 |> increment 0
    { content = [2,0,0,0], size = 4 }

-}
increment : Int -> Vector -> Vector
increment k v =
    if k < 0 || k >= v.size then
        v

    else
        set k (get k v + 1) v
            |> resetFrom (k + 1)
