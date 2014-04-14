bff-mono
========

This package is an implementation of a paper: 

    Kazutaka Matsuda and Meng Wang: 
      Bidirectionalization for Free with Runtime Recording: 
      Or, a Light-Weight Approach to the View Update Problem

The package provides an automatic way to construct a pair of functions
that constitutes a so-called lens, more precisely, a bidirectional
transformation, from a uni-directional transformation (NB: as parsers
and parser-combinators differ, bidirectional transformations and
lenses differ).

For the purpose, the package provides a type class `PackM`. Suppose
one defines a transformation `h` of type
     
    h :: forall a m. PackM c a m => [a] -> m [a] 

for some concrete type c. Then, we can derive a bidirectional
transformation from it by using `fwd` and `bwd` as

    fwd h :: [c] -> [c]  -- called get or getter 
    bwd h :: (MonadError e n, Error e) => [c] -> [c] -> n [c] -- called put or setter 

Here, we used lists as an example. However, `fwd` and `bwd` can be
applicable to any lawful `Traversable`s.

The package can be seen as an extension of the [bff
package](http://hackage.haskell.org/package/bff). Unlike the original
work, our framework can handles transformations that constructs
elements and compares something with newly-constructed
elements. `Examples.hs` includes some XML transformations that are
implemented as functions of type `forall a m. PackM L a m => Tree a ->
m (Tree a)` where `L` is a label type for XML.

  
How to Build
------------

Since the package is managed with cabal. Then, you can build the package 
as other cabal packages, as:

    $ cabal configure 
    ...
    $ cabal build 
    ... 
    $ cabal install 
    ...

Example 
-------

We demonstrate our library by using an XML transformation example
taken from [XML Query Use Cases](http://www.w3.org/TR/xquery-use-cases/).

To demonstrate our library, we firstly load `XMLExamples.hs` located
at `Examples`.

    $ cd Examples 
    $ ghci XMLExamples.hs 
    ...
    *Main> 

Let us consider an XML data `test_src`. 

    *Main> pretty $ test_src 
    <bib>
        <book year="1994">
            <title>TCP/IP Illustrated</title>
            <author><last>Stevens</last><first>W.</first></author>
            <publisher>Addison-Wesley</publisher>
            <price>65.95</price>
        </book>
        <book year="1992">
            <title>Advanced Programming in the Unix environment</title>
            <author><last>Stevens</last><first>W.</first></author>
            <publisher>Addison-Wesley</publisher>
            <price>65.95</price>
        </book>
        <book year="2000">
            <title>Data on the Web</title>
            <author><last>Abiteboul</last><first>Serge</first></author>
            <author><last>Buneman</last><first>Peter</first></author>
            <author><last>Suciu</last><first>Dan</first></author>
            <publisher>Morgan Kaufmann Publishers</publisher>
            <price>39.95</price>
        </book>
        <book year="1999">
            <title>
                The Economics of Technology and Content for Digital TV
            </title>
            <editor>
                <last>Gerbarg</last>
                <first>Darcy</first>
                <affiliation>CITI</affiliation>
            </editor>
            <publisher>Kluwer Academic Publishers</publisher>
            <price>129.95</price>
        </book>
    </bib>
  
Query Q1 in XMP Use Case extracts books with titles that are published
after 1991. `XMLExamples.hs` contains a version of the query in the intended type.

    *Main> :t q1 
    q1 :: PackM L a m => Tree a -> m (Tree a)

If we apply `q1` to `test_src`, we get the following.

    *Main> pretty $ fwd q1 test_src
    <bib>
        <book year="1994"><title>TCP/IP Illustrated</title></book>
        <book year="1992">
            <title>Advanced Programming in the Unix environment</title>
        </book>
    </bib>

Suppose that we want to change the second book to `Advanced
Programming in the Unix Environment` (`e` is capitalized) in the
transformed data and want to propagate the change to the original
input of `q1`. This is done by `bwd` we provide. 

Let `test_view_q1` is the updated view. 

    *Main> pretty $ test_view_q1
    <bib>
        <book year="1994"><title>TCP/IP illustrated</title></book>
        <book year="1992">
            <title>Advanced Programming in the Unix Environment</title>
        </book>
    </bib>

Then, we can propagate the change by using `bwd`, as follows. 

    *Main> pretty $ either error id $ bwd q1 test_src test_view_q1
    <bib>
        <book year="1994">
            <title>TCP/IP illustrated</title>
            <author><last>Stevens</last><first>W.</first></author>
            <publisher>Addison-Wesley</publisher>
            <price>65.95</price>
        </book>
        <book year="1992">
            <title>Advanced Programming in the Unix Environment</title>
            <author><last>Stevens</last><first>W.</first></author>
            <publisher>Addison-Wesley</publisher>
            <price>65.95</price>
        </book>
        <book year="2000">
            <title>Data on the Web</title>
            <author><last>Abiteboul</last><first>Serge</first></author>
            <author><last>Buneman</last><first>Peter</first></author>
            <author><last>Suciu</last><first>Dan</first></author>
            <publisher>Morgan Kaufmann Publishers</publisher>
            <price>39.95</price>
        </book>
        <book year="1999">
            <title>
                The Economics of Technology and Content for Digital TV
            </title>
            <editor>
                <last>Gerbarg</last>
                <first>Darcy</first>
                <affiliation>CITI</affiliation>
            </editor>
            <publisher>Kluwer Academic Publishers</publisher>
            <price>129.95</price>
        </book>
    </bib>

In `XMLExamples.hs`, you can see that it is not so burden to write `q1`
with the required type. 

You can find more examples in the `Examples` directory. 


