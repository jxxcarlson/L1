module Data.Example exposing (text)


text =
    """



# Examples: the L1 Markup Language

[image https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]

[image width:200 placement:left https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]

[b Errors.] Look at the examples below.  The text has errors, as noted in the rendered version.  We plan to have better error handling soon, as in Camperdown itself.

This is code `a[i] = 1 (LOL!)

This [i is] a [b real test! Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in augue eget felis rhoncus ullamcorper sed pulvinar sapien.

[b Lisp-like functions.]

The text `[fontRGB 255 0 255 foo]` renders as
[fontRGB 255 0 255 foo].  Think of `fontRGB` and a function whose arguments in this example are the elements of the list `[255 0 255 foo]`.  Functions, or more properly, functional expressions, can be nested, as in  `[fontRGB 255 0 255 foo [b bar]]` which renders as [fontRGB 255 0 255 foo [b bar]].


[b Markdown-type stuff]

Below is a small assortment.   Compare the source and rendered text to see what is going on.

[item [link NYT https://nytimes.com]]

[item sss  [link https://washingtonpost.com]]



[item [red EXAMPLE:] This [i is] a [b real] test of [b [strike this], [underline that], and the [violet other stuff]].]

[item code: `a[i] = b[i] + 1`.]

[item This is [red red meat].  [gray (We shouldn't eat so much of it.)]]

[item Pythagoras said that $a^2 + b^2 = c^2$. Wow! What a dude!!]

[item In class, we learned that]

[mathblock \\int_0^1 x^n dx = \\frac{1}{n+1}]


"""
