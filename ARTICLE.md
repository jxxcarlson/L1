# Fault-tolerant Parsing

Fault-tolerant parsing has been studied by many ... XXXX.  The approach
taken is based on Matthew Griffiths' work in [elm-markup](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/), which introduces
the notion of a *TextCursor* and its further
development in [Brilliant.org's](https://brilliant.org) Camperdown parser.
For purposes of exposition, we will discuss the main ideas in the context
of fault-tolerant parser for a simple markup language which we shall
call L1.  It can be thought of as a kind of mini-Camperdown. 

Both Camperdown and L1 are written in [Elm](htts://elm-lang.org), and both use the parser combinators of the [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/) library. The code base for L1 is small, about 1400 line, so that with this article in hand, one understand not only the principles, but also the main details of implementation of a fault-tolerant parser.  The core TextCursor module, the largest of the bunch, weighs in at a bit over 200 lines of code.

Outline ...

## The L1 Markup Language.


The main syntactic notion in **L1** is the *element:*

- `[i this is italic]` => *this is italic*

- `[image https://birds/parrot.jpg]` => display an image

- `[i This is a [b really] big deal]` — Elements can be nested.

- `[math a^2 + b^2 = c^2]` => Display the Pythagorean formula.

- `[heading2 Blue-green algae]` => section of level 2

An L1 document is a mixture of plain text and elements.
In addition there are certain features which may be viewed as syntactic
sugar but which are convenient for authors.  First, section headers may 
be written as in Markdown with leading hashmarks.  Second, inline code can be set off with backticks and inline math can be written as in LaTeX with enclosing dollar signs.  Third, there is the notion of a *block,* e.g.,

```
|mathblock
\int_0^1 x^n dx
  =
\frac{1}{n+1}
```

An ordinary block consists of the pipe symbol `|` at the beginning of a line, followed immediately by the name of the block.  A block must have one or more blank lines above and below.
This example is functionally equivalent to 

```
[mathblock \int_0^1 x^n dx = \frac{1}{n+1}]
```

However, the fact that a block is terminated by a blank line makes error-handling much easier to achieve, especially in the context of interactive editing. *((Should we change this to "two or more blank lines"?  Then blocks can handle paragraphs.))*

In addition to ordinary blocks, there are verbatim blocks, e.g.,

```
||codeblock
enclose : String -> String -> String
enclose a b =
    a ++ b ++ a                
 ```
The body of a verbatim block is not parsed.




## AST for L1

Below is the type of the AST for L1 as found in module L1.AST. Both here and further on we give slightly simplified versions.

```
type Element
    = Text String 
    | Element Name Element 
    | Verbatim VerbatimType String 
    | EList (List Element) 

```

where `type Name = Name String`.

Module L1.Parser exposes a function

```
parse : String -> Element
```

It is a recursive descent parser written using the combinators of 
[elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/). Here are some examples:
 
- `parse "foo"` => `Text "foo"
- `parse "[i foo]"` => `Element (Name "foo") (Text "foo")`
- `parse "'a[i] = 0'"` => `Verbatim Code ("a[i] = 0")`

In the last example, we really mean backtick, not `'`, but Markdown can't handle that.

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


### The GOOD case

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

- the *scanpoint,* an index int the source text.

- the *parsed* text, a list of AST values

- the *complete* AST, a list of AST values to which others may be added.

- a *stack* of items where each item holds a string and some additional information, such as the location of this string in the source text.

In the example, we proceed as follows,


1. All parts of the text cursor are empty/zero except *source.* The scan point has value 0, i.e, it points to the first character of the source.

2. The scan point is moved to the next mark, the first opening bracket in the source.  The text *The fish* between the previous and current marks is parsed and stored in *parsed.* 

3. The scan point is advanced once again.  Because it initially pointed at an open bracket `[`, the intervening text *i was* is pushed onto the stack.
In addition, the fact that we pushed text that began with an open bracket
is recorded.  We can think of the stack item as a pair `('[', "i was")`.

4. The scan point is moved across the symbol `]` and pushed onto the stack
as something like the pair `(']', ?)`.  The stack is now `[('[', "i was"), (']', ?)]`, or in shorthand, `[]`.  The brackets match and so the top two elements can be popped, put together, parsed and stored in the list *parsed.*. 

  The scanner knows that if items can be popped off the stack, they can be
put together and parsed without error.

5. Like (3), but repeated with *b very*

6. Like (4), but this time *b very* is assembled, parsed, and added to *parsed.*

7. The text *tasty.* is parsed and added to parsed.

At this point the state of the text cursor is

```
parsed = [(tasty.), (b very), (i was), (The fish)]
stack = [ ]
```

Here `(x)` means `parse x)=`.  Thus `(The fish)` is really text element
`[Text "The fish"]` and  `(i was)` is really `Element (Name "i") (Text "was")`.  The fact that the stack is empty means that all of the text was parsed. We can now commit the cursor, transferring the 


return the list *parsed* in reversed form: a list of valid AST elements representing the source text.

### The BAD case

Consider next the BAD case.  The final state of the text cursor, now displaying location information, is

```
parsed = [(The fish)]
stack = [('[', "i was", 9), ('[', "b very", 16), (']', ?, 22)]
```

The *characteristic* of the above stack is the string `"[[]"`. Look at the first character '[', an open bracket, and scan forward to find the first matching close bracket.  If one is found, remove it and remove the first character.  This is a *basic reduction*.  Thus we have `"[[]"` -> `"["` and the latter cannot be further reduced. By contrast, we have `"[[]]"` -> `"[]"` -> `""` and also `"[][]"` -> `"[]"` -> `""`.  Let us call the final
result the *residue* of the characteristic.  The residue gives information about what the error is, e.g., no error if the residue is the empty string, an unclosed open bracket if it is `"["`.

We say that a stack is *reducible* if its characteristic is the empty string. Reducible stacks are the ones that can be assembled into a valid AST element as was done in the GOOD case.  Think of reduction as a kind of inexpensive trial assembly that guarantees that assembly will succeed, just as type-checking guarantees that evaluation will succeed.


The main problem of this article now presents itself: *what do we do in the
case of a non-reducible stack?* If 


For the answer, we recall that items pushed on to the stack contain a record of the location of the snippet of source text pushed as a part of the full source text. 



We now have to explain that when an item is pushed onto the stack, so is the
current value of the scanpoint is pushed as well.  In the case at hand, it is apparent that the error ((BETTER EXPLANATION)) goes back to the first element pushed onto the stack, the *stack bottom*.  We read the stored value of .... ((TO BE CONTINUED)).



## Parsers




