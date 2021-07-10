module Data exposing (initialText)


initialText =
    """








[image caption:Camperdown https://upload.wikimedia.org/wikipedia/commons/2/20/Camperdown_Elm_Prospect_Park_Brooklyn.jpg]

# Fault-Tolerant Parsing

The goal of this article is to explain  how one can implement a fault-tolerant parser for a simple markup language which we will call [b L1].
To put the notion of fault-tolerance in context, recall that the task of a  parser reads is to read source text in some language, then convert it to an abstract syntax tree (AST).  Classical parsers act like a dumb pipe, consuming the source text and producing the AST in one go, but bailing out when an error is encountered. Such behavior is not suited for interactive language editors, be they for programming languages or markup languages.  In an interactive environoment, the source text will pass in and out of an error state, potentially  on each keystroke. A fault-tolerant parser should in all cases return an abstract syntax tree that makes sense.  This means that (a) most of the text is parsed as one expects, e.g., a new error at the halfway point does not necessarily destroy the latter half which has already been correctly parsed (b) error text is converted into a valid node of the AST which displays the text in question, signals that it is an error, and gives some insight into the nature of the error.


The strategy for fault-tolerant parsing discussed here is based on Matt Griffith's project [link "mdgriffith/elm-markup" https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/], in which he introduced the notion of [i TextCursor].  Building on Griffith's work, [link "Brillant.org" https://brilliant.org] developed a configurable fault-tolerant parser, Camperdown, for its internal authoring tools.

The Camperdown parser is versatile and can be configured for applications ranging from Markdown-style languages to interactive story-telling (see XXX).  Camperdown is also relatively complex.  The aim here, then, is to present the main ideas of Camperdown in a simple yet nontrivial context.  Here are a few sentences in the markup language [b L1] that we shall use for this exposition:

(a) `This [highlight is [b not] a very good] test.`

(b) `Pythagoras said that $a^2 + b^2 = c^2$. Wow! What a dude!!`

These are rendered as

(a) This [highlight is [b not] a very good] test.

(b) Pythagoras said that $a^2 + b^2 = c^2$. Wow! What a dude!!


## The Main Idea

In its simplest form, sentences in [b L1] consist  of [i elements]
such as `[i this is italic]` interspersed with ordinary "unmarked"
text, e.g. `[i this is italic] but that is not.` An AST that
can express such sentences is

Consider first a simple phrase such as `[b this is a test]`.

once again the sentence `This [highlight is [b not] a very good] test.`
All parsers do their work by scanning from left to right.


## References


[link "Error recovery with parser combinators"  https://eyalkalderon.com/blog/nom-error-recovery/]






"""


stuff =
    """

# Introduction to [red Chemistry] and Physics

# Introduction to [red Chemistry] and [blue Physics]

[image https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]

[image width:200 placement:left https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]

This [i is] a [b real test! Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in augue eget felis rhoncus ullamcorper sed pulvinar sapien. Integer eget felis id diam mollis cursus. In quis feugiat dolor. Mauris sed justo vel risus pharetra dapibus. Vivamus gravida, metus finibus iaculis blandit, sem sapien porttitor lacus, sed sagittis arcu dolor at velit. Sed nibh orci, volutpat a mi nec, aliquam laoreet mauris. Donec ligula neque, gravida eu pharetra eu, aliquam sed mauris. Nulla vitae elit tempus, euismod enim at, vulputate tortor. Vestibulum at efficitur dolor. Cras porta, metus vel lacinia eleifend, orci sapien laoreet ante, at pharetra ante augue non dui. Suspendisse sed velit eget felis pretium feugiat eget sed mauris.

[fontRGB 255 0 255 foo]

[fontRGB 255 0 255 foo [b bar]]

[link NYT https://nytimes.com]

[link https://washingtonpost.com]

This [i is] a [b real] test of [strike this], [underline that], and the [violet other stuff].

[red THE FOLLOWING IS MESSED UP:] This [i is] a [b real] test of [b [strike this], [underline that], and the [violet other stuff]].

This is code: `a[i] = b[i] + 1`.

This is [red red meat].  [gray (We shouldn't eat so much of it.)]

Pythagoras said that [math a^2 + b^2 = c^2].

Pythagoras said that $a^2 + b^2 = c^2$. Wow! What a dude!!

In class, we learned that

[mathblock \\int_0^1 x^n dx = \\frac{1}{n+1}]

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in augue eget felis rhoncus ullamcorper sed pulvinar sapien. Integer eget felis id diam mollis cursus. In quis feugiat dolor. Mauris sed justo vel risus pharetra dapibus. Vivamus gravida, metus finibus iaculis blandit, sem sapien porttitor lacus, sed sagittis arcu dolor at velit. Sed nibh orci, volutpat a mi nec, aliquam laoreet mauris. Donec ligula neque, gravida eu pharetra eu, aliquam sed mauris. Nulla vitae elit tempus, euismod enim at, vulputate tortor. Vestibulum at efficitur dolor. Cras porta, metus vel lacinia eleifend, orci sapien laoreet ante, at pharetra ante augue non dui. Suspendisse sed velit eget felis pretium feugiat eget sed mauris.
"""
