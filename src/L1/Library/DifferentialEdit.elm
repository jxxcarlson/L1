module L1.Library.DifferentialEdit exposing (..)

import L1.Library.Differ


type alias EditRecord a b =
    { chunks : List String
    , parsed : List { chunk : String, tree : List a }
    , rendered : List b
    }


init : (String -> List String) -> (String -> List a) -> (List a -> b) -> String -> EditRecord a b
init chunker parser renderer text =
    let
        chunks =
            chunker text

        parsed =
            List.map (\p -> { chunk = p, tree = parser p }) chunks

        rendered =
            List.map (.tree >> renderer) parsed
    in
    { chunks = chunks, parsed = parsed, rendered = rendered }


{-| The update function takes an EditRecord and a string, the "text",
breaks the text into a list of logical paragraphs, diffs it with the list
of paragraphs held by the EditRecord, uses `differentialRender` to
render the changed paragraphs while copying the unchanged rendered paragraphsto
prodduce an updated list of rendered paragraphs. The 'differentialRender'
accomplishes this using the transformer. The seed is used to produces
a differential idList. This last step is perhaps unnecessary. To investigate.
(This was part of an optimization scheme.)
-}
update : (String -> List String) -> (String -> List a) -> (List a -> b) -> EditRecord a b -> String -> EditRecord a b
update chunker parser renderer editRecord text =
    let
        newChunks =
            chunker text

        diffRecord =
            Library.Differ.diff editRecord.chunks newChunks

        parsed =
            differentialCompiler parser renderer diffRecord editRecord
    in
    { chunks = chunks, parsed = parsed, rendered = rendered }


differentialCompiler :
    (String -> List LatexExpression)
    -> (String -> a)
    -> Library.Differ.DiffRecord
    -> EditRecord a b
    -> ( List ( String, List LatexExpression ), List a )
differentialCompiler parser renderer diffRecord editRecord =
    let
        ii =
            List.length diffRecord.commonInitialSegment

        it =
            List.length diffRecord.commonTerminalSegment

        initialSegmentParsed =
            List.take ii editRecord.astList

        terminalSegmentParsed =
            takeLast it editRecord.astList

        middleSegmentParsed =
            List.map (\p -> ( p, parser p )) diffRecord.middleSegmentInTarget

        initialSegmentRendered =
            List.take ii editRecord.renderedParagraphs

        terminalSegmentRendered =
            takeLast it editRecord.renderedParagraphs

        middleSegmentRendered =
            -- TODO: to improve!
            List.map renderer diffRecord.middleSegmentInTarget
    in
    ( initialSegmentParsed ++ middleSegmentParsed ++ terminalSegmentParsed
    , initialSegmentRendered ++ middleSegmentRendered ++ terminalSegmentRendered
    )


takeLast : Int -> List a -> List a
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse
