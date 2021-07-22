# Fault-tolerant Parsing

Fault-tolerant parsing has been studied by many ... XXXX.  The approach
taken is based on Matthew Griffiths' work in [elm-markup](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) and it further
development in [Brilliant.org's](https://brilliant.org) Camperdown parser.
For purposes of exposition, we will discuss the main ideas in the context
of fault-tolerant parser for a simple markup language which we shall
call L1.  It can be thought of as a kind of mini-Camperdown. 

Both Camperdown and L1 are written in [Elm](htts://elm-lang.org), and both use the parser combinators of the [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/) library. The code base for L1 is small, so that with this article in hand, one understand not only the principles, but also the main details of implementation.  The core TextCursor module weighs in at a bit over 200 lines of code.

Outline ...

## The L1 Markup Language.


The core markup construct in **L1** is the *element:*

- `[i this is italic]` => *this is italic*

- `[image https://birds/parrot.jpg]` => display an image

- `[i This is a [b really] big deal]` — Elements can be nested.

- `[math a^2 + b^2 = c^2]` => Display the Pythagorean formula.

- `[heading2 Blue-green algae]` => section of level 2

An **L1** document is a mixture of plain text and elements.

In addition there are certain features which may be viewed as syntactic
sugar but which are convenient for authors.  First, section headers may 
be written as in Markdown with leading hashmarks.  Second, inline code can be set of with backticks and inline math can be written as in LaTeX with enclosing dollar signs.  Third, there is the notion of a block, e.g.,

```
|mathblock
\int_0^1 x^n dx
  =
\frac{1}{n+1}
```

An ordinary block consists of the pipe symbol `|` at the beginning of a line, followed immediately by the name of the block, in this case *mathblock*.  A block must have one or more blank lines above and below.
This example is functionally equivalent to 

```
[mathblock \int_0^1 x^n dx = \frac{1}{n+1}]
```

However, the fact that a block is terminated by a blank line makes error-handling much easier to achieve, especially in the context of interactive editing. 

In addition to ordinary blocks, there are verbatim blocks, e.g.,

```
||codeblock
enclose : String -> String -> String
enclose a b =
    a ++ b ++ a                
 ```
The body of a verbatim block is not parsed.

## AST and parser for L1

Below is the type of the L1 AST as found in Parser.AST. Both here and further on we give slightly simplified versions.

```
type Element
    = Text String 
    | Element Name Element 
    | Verbatim VerbatimType String 
    | EList (List Element) 

```

The parser itself is found in module Parser.Parser, which exposes a function

```
parse : String -> Element
```




## Basic notions

The elm-markup, Camperdown, and **L1** parsers are based on just three notions:

- Chunking
- Expectations stack
- Text Cursor

By chunking we mean that the source text is divided into pieces which are in principle parsable.  These can then be parsed independently, and errors in one piece will not affect errors in other pieces.  

 An advantage of chunking is that it also permits one to do differential parsing and rendering.  Suppose that a document consists of pieces A B C D E. Suppose that the author makes a change to C.  Then one can arrange things so that the entire document can be rendered by re-parsing and re-rendering C.  For long documents this procedure is far faster than re-parsing and r-rendering the entire text — a task that has to be done on each character stroke.
 
To understand the notions of expectations stack and text cursor, consider the following example:

```
GOOD: The fish [i was] [b very] tasty.
```

and its companion, which is invalid markup:

```
BAD: The fish [i was [b very] tasty.
```

How can we best handle the invalid text?  One solution is to stop the 
parser and emit a message such as `fatal error`, or more 
informative and less offensive, `error at line 7, column 10`.
But one can do much better ((SCREENSHOT)).  The parser can be rigged so
as to keep going, rendering most of the text in an intelligent way, and signaling both the presence and nature of the error.

To accomplish this, we imagine scanning a piece of the source text from
left to write, cutting it into pieces as we go, and pushing those
pieces on a stack.  The scan point is written as `^`. The table
below lists the decision points where some action must be taken.
These are the points at which the scanner encounters a language
symbol, in this case either `[` or `]`.

```
1: ^The fish [i was] [b very] tasty.   START
2: The fish ^[i was] [b very] tasty.   ADD
3: The fish [i was^] [b very] tasty.   PUSH
4: The fish [i was]^ [b very] tasty.   POP
5: The fish [i was]  [b very^] tasty.  PUSH
6: The fish [i was] [b very]^ tasty.   POP
7: The fish [i was] [b very] tasty.^   ADD
```

The scanner maintains several data structures as part of a "text cursor."

- the *source* text

- the *scanpoint,* an index int the source text

- the *parsed* text, a list of AST values ((explain above))

- a *stack* of strings

In the example, we proceed as follows,


1. All parts of the text cursor are empty/zero except *source.*

2. The scan point is moved to the next mark.  The text *The fish* between the previous and current marks is parsed and stored in *parsed.* 

3. The scan point is advanced once again.  Because it initially pointed at an open bracket `[`, the intervening text *i was* is pushed onto the stack.
In addition, the fact that we pushed text that began with an open bracket
is recorded.  We can think of the stack element as a pair `('[', "i was")`.

4. The scan point is moved across the symbol `]` and pushed onto the stack
as something like the pair `(']', ?)`.  The stack is now `[('[', "i was"), (']', ?)]`, or in shorthand, `[]`.  The brackets match and so the top two elements can be popped, put together, parsed and stored in the list *parsed.*. 

  The scanner knows that if items can be popped off the stack, they can be
put together and parsed without error.

5,6. The same process is repeated with *b very*

7. The text *tasty.* is parsed and added to parsed.

At this point we have

```
parsed = [(tasty.), ([b very]), ([i was]), (The fish)]
stack = [ ]
```

Here `(x)` means `(parse x)`.  Thus `(The fish)` is a text element, 
`([i was])` is an element with name `i` and body the text `was`, etc. ((EXPLAIN BETTER)). The fact that the stack is empty means that all of the text was parsed. We return the list *parsed* in reversed form: a list of valid AST elements representing the source text.

Consider next the BAD case.  The final state of the text cursor is

```
parsed = [(The fish)]
stack = [('[', "i was"), ('], "??"), ('[', "b very"), (']'. ?)]
```

The simple view of the stack is `[[]`.  It is not reducible to the empty
stack.  What to do?  Whe items are pushed onto the stack, we know that 

We now have to explain that when an item is pushed onto the stack, so is the
current value of the scanpoint is pushed as well.  In the case at hand, it is apparent that the error ((BETTER EXPLANATION)) goes back to the first elmement pushed onto the stack, the *stack bottom*.  We read the stored valud of .... ((TO BE CONTINUED)).
