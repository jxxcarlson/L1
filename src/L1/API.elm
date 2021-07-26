module L1.API exposing (renderDocument, renderDocument_)

import Element as E
import L1.Parser.Document
import L1.Render.Elm exposing (RenderArgs)


renderDocument_ : RenderArgs -> Int -> String -> List (List (E.Element msg))
renderDocument_ renderArgs generation document =
    document
        |> L1.Parser.Document.parse generation
        |> List.map (\para -> L1.Render.Elm.renderList { renderArgs | generation = generation } para)


renderDocument : RenderArgs -> Int -> String -> List (E.Element msg)
renderDocument renderArgs generation sourceText =
    List.map (\para -> E.paragraph [] para) (renderDocument_ renderArgs generation sourceText)
