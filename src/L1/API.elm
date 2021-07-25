module L1.API exposing (..)

import Element exposing (Element)
import L1.Parser.Document
import L1.Render.Elm exposing (RenderArgs)


renderDocument : RenderArgs -> Int -> String -> List (List (Element msg))
renderDocument renderArgs generation document =
    document
        |> L1.Parser.Document.parse generation
        |> List.map (\para -> L1.Render.Elm.renderList { renderArgs | generation = generation } para)
