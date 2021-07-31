module L1.Render.LaTeX exposing (testDoc, transform, transformDocument)

import Dict exposing (Dict)
import L1.Library.Utility
import L1.Parser.AST as AST exposing (Element(..), Element_(..), Name(..), VerbatimType(..))
import L1.Parser.Document
import L1.Parser.Error exposing (Context(..), Problem(..))
import Parser.Advanced


type alias ParseError =
    Parser.Advanced.DeadEnd Context Problem


type alias RenderArgs =
    { width : Int
    }


type alias FRender =
    RenderArgs -> String -> List Element -> String


type alias RenderElementDict =
    Dict String FRender


testData =
    """### Text Cursor

To explain the  scanning/cursor idea, let us consider the two snippets

(a) `This [i is] a [b real] test!`
"""


testDoc =
    """In L1, we say `[b bold text]` to make bold text, and for italic, we say `[i italic text]`. Elements can be nested as in `[i italic text is very [b bold]]`, which renders as"""


renderElementDict : RenderElementDict
renderElementDict =
    Dict.fromList
        [ ( "i", italic )
        , ( "b", bold )
        , ( "strike", strike )
        , ( "red", macro0 "red" )
        , ( "blue", macro0 "blue" )
        , ( "gray", macro0 "gray" )
        , ( "code", code )
        , ( "quoted", quoted )
        , ( "math2", math )
        , ( "m", math )
        , ( "mathblock", mathblock )
        , ( "mb", mathblock )
        , ( "link", link )
        , ( "image", image )

        -- , ( "heading", heading )
        , ( "item", item )
        , ( "heading1", heading1 )
        , ( "heading2", heading2 )
        , ( "heading3", heading3 )
        , ( "heading4", heading4 )
        , ( "codeblock", codeblock )
        ]


transformDocument : String -> String
transformDocument doc =
    doc
        |> L1.Parser.Document.parse 0
        |> List.map (transformList { width = 600 })
        |> String.join "\n\n"
        |> (\data -> preamble ++ data ++ "\n\\end{document}")


{-| A standard preamble
-}
preamble : String
preamble =
    """
\\documentclass[11pt, oneside]{article}

%% Packages
\\usepackage{geometry}
\\geometry{letterpaper}
\\usepackage{changepage}   % for the adjustwidth environment
\\usepackage{graphicx}
\\usepackage{wrapfig}
\\graphicspath{ {images/} }
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{amscd}
\\usepackage{hyperref}
\\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    filecolor=magenta,
    urlcolor=blue,
}
\\usepackage{xcolor}
\\usepackage{soul}


%% Commands
\\newcommand{\\code}[1]{{\\tt #1}}
\\newcommand{\\ellie}[1]{\\href{#1}{Link to Ellie}}
\\newcommand{\\image}[1]{\\includegraphics[width=3cm]{#1}}

\\newcommand{\\imagecenter}[3]{{
   \\medskip
   \\begin{figure}
   \\centering
    \\includegraphics[width=12cm,height=12cm,keepaspectratio]{#1}
    \\vglue0pt \\par {#2}
    \\end{figure}
    \\medskip
}}

\\newcommand{\\imagefloatright}[3]{
    \\begin{wrapfigure}{R}{0.30\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}

\\newcommand{\\imagefloatleft}[3]{
    \\begin{wrapfigure}{L}{0.3-\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}

\\newcommand{\\bareitem}[1]{{\\sl #1}}
\\newcommand{\\italic}[1]{{\\sl #1}}
\\newcommand{\\strong}[1]{{\\bf #1}}
\\newcommand{\\subheading}[1]{{\\bf #1}\\par}
\\newcommand{\\xlink}[2]{\\href{{https://minilatex.lamdera.app/g/#1}}{#2}}
\\newcommand{\\red}[1]{\\textcolor{red}{#1}}
\\newcommand{\\gray}[1]{\\textcolor{gray}{#1}}
\\newcommand{\\blue}[1]{\\textcolor{blue}{#1}}
\\newcommand{\\remote}[1]{\\textcolor{red}{#1}}
\\newcommand{\\local}[1]{\\textcolor{blue}{#1}}
\\newcommand{\\highlight}[1]{\\hl{#1}}
\\newcommand{\\note}[2]{\\textcolor{blue}{#1}{\\hl{#1}}}
\\newcommand{\\strike}[1]{\\st{#1}}
\\newcommand{\\term}[1]{{\\sl #1}}
\\newtheorem{remark}{Remark}
\\newcommand{\\comment}[1]{}
\\newcommand{\\innertableofcontents}{}

%% Theorems
\\newtheorem{theorem}{Theorem}
\\newtheorem{axiom}{Axiom}
\\newtheorem{lemma}{Lemma}
\\newtheorem{proposition}{Proposition}
\\newtheorem{corollary}{Corollary}
\\newtheorem{definition}{Definition}
\\newtheorem{example}{Example}
\\newtheorem{exercise}{Exercise}
\\newtheorem{problem}{Problem}
\\newtheorem{exercises}{Exercises}
\\newcommand{\\bs}[1]{$\\backslash$#1}
\\newcommand{\\texarg}[1]{\\{#1\\}}

%% Environments
\\renewenvironment{quotation}
  {\\begin{adjustwidth}{2cm}{} \\footnotesize}
  {\\end{adjustwidth}}

% Spacing
\\parindent0pt
\\parskip5pt

\\begin{document}


"""


transformList : RenderArgs -> List Element -> String
transformList renderArgs list =
    List.map (transform renderArgs) list |> String.join " "


transform : RenderArgs -> Element -> String
transform renderArgs element =
    case element of
        Text str _ ->
            str

        Element (Name name) body _ ->
            -- renderWithDictionary renderArgs name (renderList renderArgs body)
            renderWithDictionary renderArgs name body

        Verbatim verbatimType content _ ->
            renderVerbatim verbatimType content

        Element UndefinedName body _ ->
            "Undefined element"

        Problem _ str ->
            "PROBLEM: " ++ str


renderList renderArgs elements =
    List.map (AST.map (\s -> " " ++ s) >> transform renderArgs) elements |> String.join ""


renderVerbatim verbatimType content =
    case verbatimType of
        Math ->
            "$" ++ content ++ "$"

        Code ->
            "\\code{" ++ zap content ++ "}"

        Quoted ->
            content


zap : String -> String
zap str =
    str |> String.replace "#" "\\#"


renderWithDictionary : RenderArgs -> String -> List Element -> String
renderWithDictionary renderArgs name body =
    case Dict.get name renderElementDict of
        Just f ->
            f renderArgs name body

        Nothing ->
            "[" ++ name ++ " " ++ transformList renderArgs body ++ "]"



-- TEXT STYLE


macro0 : String -> FRender
macro0 name renderArgs _ body =
    "\\" ++ name ++ "{" ++ String.trim (transformList renderArgs body) ++ "}"


type ArgOrder
    = NormalOrder
    | ReversedOrder


getArgs : List Element -> List String
getArgs body =
    List.map AST.getArgs body |> List.concat


macro2 : ArgOrder -> String -> FRender
macro2 argOrder macroName _ _ body =
    case getArgs body of
        arg1 :: arg2 :: rest ->
            case argOrder of
                NormalOrder ->
                    "\\" ++ macroName ++ "{" ++ arg1 ++ "}" ++ "{" ++ arg2 ++ "}"

                ReversedOrder ->
                    "\\" ++ macroName ++ "{" ++ arg2 ++ "}" ++ "{" ++ arg1 ++ "}"

        _ ->
            "Error rendering " ++ macroName


italic : FRender
italic =
    macro0 "italic"


bold : FRender
bold =
    macro0 "strong"


strike : FRender
strike =
    macro0 "strike"


quoted : FRender
quoted =
    macro0 "quoted"


code : FRender
code =
    macro0 "code"


link : FRender
link =
    macro2 ReversedOrder "href"



--
--heading : FRender
--heading renderArgs name body =
--    let
--        text =
--            getTextList body |> String.join " "
--
--        level =
--            text
--                |> String.words
--                |> List.filter (\s -> String.left 1 s == "#")
--                |> List.head
--                |> Maybe.withDefault ""
--                |> String.length
--
--        prefix =
--            String.repeat level "#"
--
--        title =
--            String.replace prefix "" text
--    in
--    "#" ++ prefix ++ " " ++ title
--


item : FRender
item =
    macro0 "bareitem"


heading1 : FRender
heading1 =
    macro0 "section"


heading2 : FRender
heading2 =
    macro0 "subsection"


heading3 : FRender
heading3 =
    macro0 "subsubsection"


heading4 : FRender
heading4 =
    macro0 "subheading"


image : FRender
image renderArgs name body =
    let
        url =
            getArgs body
                |> List.map String.words
                |> List.concat
                |> List.reverse
                |> List.head
                |> Maybe.withDefault "(no url)"
                |> String.replace "https://" ""
                |> String.replace "http://" ""
                |> String.replace "/" "-"
                |> String.replace "_" "-"
                |> (\x -> "image/" ++ x)
    in
    "\\" ++ "image" ++ "{" ++ url ++ "}"



-- Math


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


math : FRender
math renderArgs name body =
    "$" ++ (getTextList body |> String.join " ") ++ "$"


mathblock : FRender
mathblock rendArgs name body =
    "$$" ++ (getTextList body |> String.join " ") ++ "$$"


codeblock : FRender
codeblock rendArgs name body =
    "\\" ++ "begin{verbatim}" ++ "\n" ++ (getTextList body |> String.join " ") ++ "\n\\end{verbatim}"



-- HELPERS


toInt : String -> Int
toInt x =
    x |> String.trim |> String.toInt |> Maybe.withDefault 0


getInt : Element -> Int
getInt e =
    e |> getText2 |> toInt


getText2 : Element -> String
getText2 element =
    case element of
        Text s _ ->
            s

        _ ->
            ""


getTextList : List Element -> List String
getTextList elements =
    List.map getText2 elements


getText : Element -> Maybe String
getText element =
    case element of
        Text content _ ->
            Just content

        _ ->
            Nothing
