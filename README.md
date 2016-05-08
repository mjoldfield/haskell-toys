# haskell-toys
An attempt to abstract away the boilerplate when writing little command line toys in Haskell.

## Rationale
I find Haskell a fine tool for writing little utility programs. It’s most succinct, elegant,
and programs tend to work without much effort suprisingly often. Often I’ll just write a
single .hs file and the job’s done.

However there are a couple of issues:

* Some of the cleverer libraries e.g. JuicyPixels use the type system to great effect,
letting you work with images at a very abstract level. I find this makes it a bit harder
to just use them from e.g. the ghci prompt without peppering things with more concrete
type annotations.

* I can be quite lazy when writing such utilities. Suppose I have three images which I
want to munge in a particular way, and save to new files.
I think a good approach is to write something which
munges the filenames systematically, then iterate over the three input names. In 
practice, I’ve tended to just invoke a command three times, manually specifying
the output names.

This simple library is an attempt to solve these. It is deliberately _opinionated_
in that it provides functions which I find useful, whilst making it harder to access
things which I use less often. You might think of it as a compression scheme tailored
to give short expressions for things I personally use often.

## Contents

### [Toy.Generic](https://github.com/mjoldfield/haskell-toys/blob/master/src/Toy/Generic.hs)

* Support for transforming lists of files in a principled way.

### [Toy.JuicyPixels](https://github.com/mjoldfield/haskell-toys/blob/master/src/Toy/JuicyPixels.hs)

* Specialize to the PixelRGB8 image type.

* Combinators to help munge images.
