module Data.Article2 exposing (..)


text =
    """


# Fault-tolerant Parsing

Fault-tolerant parsing has been studied by many ... XXXX.  The approach
taken here is based on Matthew Griffiths' work in [link "elm-markup" https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/], which introduces
the notion of a [i TextCursor] and work at [link "Brilliant.org" https://brilliant.org] on a domain-specific parser by the team led by Rob Simmons.
For purposes of exposition, we will discuss the main ideas in the context
of fault-tolerant parser for a simple markup language which we
call L1.   There is a pre-alpha demo at [link "jxxcarlson.github.io" https://jxxcarlson.github.io/app/L1/] with code  [link "here" https://github.com/jxxcarlson/L1].

Both Camperdown and L1 are written in [link "Elm"  https://elm-lang.org], and both use the parser combinators of the [link "elm/parser" https://package.elm-lang.org/packages/elm/parser/latest/] library. The code base for the L1 parser is small, about 1400 lines, with the core TextCursor module, the largest of the bunch, weighing in at a bit over 200 lines. With this article in hand, one can use the code to understand not only the principles, but also the main details of implementation of a fault-tolerant parser.


## The L1 Markup Language.


The principal syntactic notion in L1 is the [i element:]

:`[i this is italic]` => [i this is italic]

: `[image https://birds/parrot.jpg]` => display an image

: `[i This is a [b really] big deal]` — Elements can be nested.

: `[math a^2 + b^2 = c^2]` => Display the Pythagorean formula.

: `[heading2 Blue-green algae]` => section of level 2

An L1 document is a mixture of plain text and elements.
In addition there are certain features which may be viewed as syntactic
sugar but which are convenient for authors.  First, section headers may
be written as in Markdown with leading hashmarks.  Second, inline code can be set off with backticks and inline math can be written as in LaTeX with enclosing dollar signs.  Third, there is the notion of a *block,* e.g.,

|| codeblock
| mathblock
\\int_0^1 x^n dx
  =
\\frac{1}{n+1}


An ordinary block consists of the pipe symbol `|` at the beginning of a line, followed immediately by the name of the block.  A block must have one or more blank lines above and below.
This example is functionally equivalent to

|| codeblock
[mathblock \\int_0^1 x^n dx = \\frac{1}{n+1}]


However, the fact that a block is terminated by a blank line makes error-handling much easier to achieve, especially in the context of interactive editing. *((Should we change this to "two or more blank lines"?  Then blocks can handle multiple paragraphs.))*

In addition to ordinary blocks, there are verbatim blocks, e.g.,

|| codeblock
|| codeblock
enclose : String -> String -> String
enclose a b =
    a ++ b ++ a

The body of a verbatim block is not parsed.




## AST for L1

Below is the type of the AST for L1 as found in module L1.AST. Both here and further on we give slightly simplified versions.

|| codeblock
type Element
    = Text String
    | Element Name Element
    | Verbatim VerbatimType String
    | EList (List Element)

type Name = Name String


Module L1.Parser exposes a function

|| codeblock
parse : String -> Element


It is a recursive descent parser written using parser combinators (see
[link "elm/parser" https://package.elm-lang.org/packages/elm/parser/latest/]). Here are some examples:


: `parse "foo"` => `Text "foo"`

: `parse "[i foo]"` => `Element (Name "foo") (Text "foo")`

: `parse "'a[i] = 0'"` => `Verbatim Code ("a[i] = 0")`

In the last example, we really mean backtick, not `'`, but Markdown can't handle that.

## Basic notions

The elm-markup, Camperdown, and **L1** parsers are based on three ideas:

: Chunking

: Expectations stack


: Text Cursor

By chunking we mean that the source text is divided into pieces which are in principle parsable.  These can then be parsed independently, and errors in one piece will not affect errors in other pieces.

 An additional advantage of chunking is that it also permits one to do differential parsing and rendering.  Suppose that a document consists of pieces A B C D E. Suppose that the author makes a change to C.  Then one can arrange things so that the entire document can be rendered by re-parsing and re-rendering C.  For long documents this procedure is far faster than re-parsing and r-rendering the entire text — a task that has to be done on each character stroke.

Chunks in L1 are sequences of non-blank lines delimited above an below by at least one blank line.


### An Example


To understand the notions of expectations stack and text cursor, consider the following example:

|| codeblock
GOOD: The fish [i was] [b very] tasty.


and its companion, which is invalid markup:

|| codeblock
BAD: The fish [i was [b very] tasty.


One way to handle invalid input is to stop the
parser and emit a message such as `fatal error`, or more
informative and less offensive, `error at line 7, column 10`.
But one can do much better.  The parser can be rigged so
as to keep going, rendering almost all of the text in an intelligent way, and signaling both the presence and nature of the error. Try out
 [link "pre-alpha demo" https://jxxcarlson.github.io/app/L1/] to see one solution to such problems.

To fix ideas, consider two examples:

: 1. `The fish [i was] [b very] tasty.`

: 2. `The fish [i was [b very] tasty.`

The first, which is valid L1 text, consists of four parts, each which can be parsed seprately: (a) `The fish`, (b) `[i was]`, (c) `[b very]`, (d) `tasty.`  The corresponding pieces in example (2) are as before, except for (b) `[i was`.  In this example the piece (b) is a syntax fragment which is not parsable.




### The GOOD case


Our task now is to find a systematic way of cutting the text into pieces, parsing them, then assembling the parts into a valid AST.  To do this, we imagine scanning the text from left to right, taking action whenever it
encounters an open or closed bracket.

|| codeblock
1: ^The fish [i was] [b very] tasty.   START
2: The fish ^[i was] [b very] tasty.   ADD
3: The fish [i was^] [b very] tasty.   PUSH
4: The fish [i was]^ [b very] tasty.   POP
5: The fish [i was]  [b very^] tasty.  PUSH
6: The fish [i was] [b very]^ tasty.   POP
7: The fish [i was] [b very] tasty.^   ADD


The scanner maintains several data structures as part of a "text cursor."

: • the [i source] text

: • the [i scanpoint,] an index int the source text.

: • the [i parsed] text, a list of AST values

: • the [i complete] AST, a list of AST values to which others may be added.

: • a [i stack] of items where each item holds a string and some additional information, such as the location of this string in the source text.

In the example, we proceed as follows,


: 1. All parts of the text cursor are empty/zero except *source.* The scan point has value 0, i.e, it points to the first character of the source.

: 2. The scan point is moved to the next mark, the first opening bracket in the source.  The text *The fish* between the previous and current marks is free of marks and so can be parsed as `Text str` for some  string `str`.
The result is stored in the *completed*  field of the text cursor.

: 3. The scan point is advanced once again.  Because it initially pointed at an open bracket `[`, the intervening text *i was* is pushed onto the stack.
In addition, the fact that we pushed text that began with an open bracket
is recorded.  We can think of the stack item as a pair `('[', "i was")`.

: 4. The scan point is moved across the symbol `]` and pushed onto the stack
as something like the pair `(']', ?)`.  The stack is now `[('[', "i was"), (']', ?)]`, or in shorthand, `[]`.  The brackets match and so the top two elements can be popped, put together, parsed and stored in the list *completed.*

:  The scanner knows that if items can be popped off the stack, they can be
put together and parsed without error.

: 5. Like (3), but repeated with *b very*

: 6. Like (4), but this time *b very* is assembled, parsed, and added to *parsed.*

: 7. The text *tasty.* is parsed and added to *completed.*

At this point the state of the text cursor is

|| codeblock
completed = [(tasty.), (b very), (i was), (The fish)]
stack = [ ]


Here `(x)` means `parse x`.  Thus `(The fish)` is really text element
`[Text "The fish"]` and  `(i was)` is really `Element (Name "i") (Text "was")`.  The fact that the stack is empty means that all of the text was parsed. We can now commit the cursor and extract the AST by reversing the list *complete.*


### The BAD case

Consider next the BAD case.  The final state of the text cursor, now displaying location information, is

|| codeblock
complete = [(The fish, 0)]
parsed = [(tasty, 24)]
stack = [('[', "i was", 9), ('[', "b very", 15), (']', ?, 22), ]


The [i characteristic] of the above stack is the string `"[[]"`. Look at the first character `[`, an open bracket, and scan forward to find the first matching closed bracket.  If one is found, remove it and remove the first character.  This is a *basic reduction*.  Thus we have `"[[]"` -> `"["` and the latter cannot be further reduced. By contrast, we have `"[[]]"` -> `"[]"` -> `""` and also `"[][]"` -> `"[]"` -> `""`.  Let us call the final
result the *residue* of the characteristic.  The residue gives information about what the error is, e.g., no error if the residue is the empty string, an unclosed open bracket if it is `"["`.

We say that a stack is *reducible* if its characteristic is the empty string. Reducible stacks are the ones that can be assembled into a valid AST element as was done in the GOOD case.  Think of reduction as a kind of inexpensive trial assembly that guarantees that actual assembly will succeed, just as type-checking guarantees that evaluation will succeed.


The main problem of this article now presents itself in concrete form: *what do we do in the case of a non-reducible stack?*  We will try the following.

: 1. Drop the bottom of the stack. If what remains is reducible, convert the dropped element into an "error node", e.g., `Element (Name "error"), Text ("unmatched '[' before 'i was'")`.  Add this element to *complete*.

: 2. Set *scanpoint = 9*, discard the contents of *parsed*, and set the scanning machinery in motion once again.

It may happen that converting the bottom of the stack to a valid node is insufficient: the truncated stack may still be irreducible.
In this case we repeat the above process until the scanning can be resumed or until the stack is exhausted.

It may also once happen that the scanner encounters a non-reducible state again.  However, since at least part of the text is moved to *complete* when this happens,  *scanpoint* advances each time, and so the process is guaranteed to terminate.

While the recovery procedure just described may not be optimal, it works, and in many cases gives good results.  Note that there is no way to determine at the outset what might be done to correct invalid input.  Indeed, consider our example once again: `The fish [i was [b very] tasty.`
The author may have meant to say any one of the following:

: 1. `The fish [i was] [b very] tasty.`

: 2. `The fish [i was [b very]] tasty.`

: 3. `The fish [i was [b very] tasty.]`

The point is to recover in such a way that the author can examine a complete, presentable, and legible version of the parsed and rendered text that the author can then correct.



### References

: 1. [link "Matt Griffith, elm-markup" https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/]

: 2. [link "Discussion on Elm discourse" https://discourse.elm-lang.org/t/parsers-with-error-recovery/6262]

: 3. [link "Error recovery with parser combinators" https://eyalkalderon.com/blog/nom-error-recovery/]










"""
