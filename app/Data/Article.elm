module Data.Article exposing (text)


text =
    """





[image caption:Camperdown https://upload.wikimedia.org/wikipedia/commons/2/20/Camperdown_Elm_Prospect_Park_Brooklyn.jpg]

# Fault-Tolerant Parsing

The goal of this article is to explain  how one can implement a fault-tolerant parser for a simple markup language which we will call [b L1].
To put the notion of fault-tolerance in context, recall that the task of a  parser reads is to read source text in some language, then convert it to an abstract syntax tree (AST).  Classical parsers act like a dumb pipe, consuming the source text and producing the AST in one go, but bailing out when an error is encountered. Such behavior is not suited for interactive language editors, be they for programming languages or markup languages.  In an interactive environoment, the source text will pass in and out of an error state, potentially  on each keystroke. A fault-tolerant parser should in all cases return an abstract syntax tree that makes sense.  This means that (a) most of the text is parsed as one expects, e.g., a new error at the halfway point does not necessarily destroy the latter half which has already been correctly parsed (b) error text is converted into a valid node of the AST which displays the text in question, signals that it is an error, and gives some insight into the nature of the error.


The strategy for fault-tolerant parsing discussed here is based on Matt Griffith's project [link "mdgriffith/elm-markup" https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/], in which he introduced the notion of [i TextCursor].  Building on Griffith's work, [link "Brillant.org" https://brilliant.org] developed a configurable fault-tolerant parser, Camperdown, for its internal authoring tools.

The Camperdown parser can be configured for applications ranging from Markdown-style languages to a kind of mini-LaTeX to interactive story-telling (see XXX).  The aim here is to present the main ideas of Camperdown in a simple yet nontrivial context that will be helpful both on its own and as a warmup to understanding and using Camperdown. The  codebase for [b L1] is small, with the core `textCursor` module, the largest of the bunch,  weighing in at 300 lines of code. Here are a few sentences in [b L1]:

[item (a) `This [highlight is [b not] a very good] test.`]

[item (b) `Pythagoras said that $a^2 + b^2 = c^2$. Wow! What a dude!!`]

These are rendered as

[item (a) This [highlight is [b not] a very good] test.]

[item (b) Pythagoras said that $a^2 + b^2 = c^2$. Wow! What a dude!!]



# The Main Idea

## Isolation

## Scratch Work



In its simplest form, sentences in [b L1] consist  of [i elements]
such as `[i this is italic]` interspersed with ordinary "unmarked"
text, e.g. `[i this is italic] but that is not.` An AST that
can express such sentences is

Consider first a simple phrase such as `[b this is a test]`.

once again the sentence `This [highlight is [b not] a very good] test.`
All parsers do their work by scanning from left to right.



# References


[link "Error recovery with parser combinators"  "https://eyalkalderon.com/blog/nom-error-recovery/"]




"""
