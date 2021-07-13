module Data.Example exposing (text)


text =
    """







# The L1 Markup Language

[b L1] is a markup language witha syntax somewhat like Lisp, but with square brackets instead of parentheses.  To make bold text, we say this: `[b bold text]`, and for italic, we say `[i italic text]`.  These can be nested as in
`[i italic text is very [b bold]]`.There are other constructs as well.  Titles and section headings, for example, are done as in Markdown:

` # The L1 Markup Language`

Below are more examples.  We begin with images.  The best way to see how [b L1] works is to compare the source text with the rendered text.



## Images

[image width:80 placement:left https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]


[image width:200 placement:left https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]

[image https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]



## Errors

Look at the example below, where the source text is labeled (1) and the rendered version is labeled (2).  There should be a right bracket after [i real]. The error is flagged in rendered text, but the subsequent italicized text is unaffected.  This error-tolerance is a feature which [b L1] derives from Camperdown (see the [i Article] tab).

(1) `This [i is] a [b real test! [i Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in augue eget felis rhoncus ullamcorper sed pulvinar sapien.]`


(2) This [i is] a [b real test! [i Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in augue eget felis rhoncus ullamcorper sed pulvinar sapien.]

## Lisp-like functions

The text `[fontRGB 255 0 255 foo]` renders as
[fontRGB 255 0 255 foo].  Think of `fontRGB` and a function whose arguments here are the elements of the list `[255 0 255 foo]`.  Functions, or more properly, functional expressions, can be nested, as in  `[fontRGB 255 0 255 foo [b bar]]` which renders as [fontRGB 255 0 255 foo [b bar]].

##  Markdown-type stuff

Below are some Markdown-like examples.   Compare the source and rendered text to see what is going on.

### Links

[item [link NYT https://nytimes.com]]

### Code

[item code: `a[i] = b[i] + 1`.]

[item This is [red red meat].  [gray (We shouldn't eat so much of it.)]]



### Math

[item Pythagoras said that $a^2 + b^2 = c^2$. Wow! What a dude!!]

[item In class, we learned that]

[mathblock \\int_0^1 x^n dx = \\frac{1}{n+1}]








"""
