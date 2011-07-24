Overview
========

IndexedSet provides fast querying on sets by providing indexing on arbitrary attributes.

Querying on indices is O(log n), rather than the O(n) performance you would get with a regular linear search on a set (i.e. Data.Set.filter). The downfall of this is that inserts into the set become a bit more expensive; it adds O(log n) for every insert operation.

See Main.hs for a simple example application that gives an overview of the basic functionality, and take a look at the source code in IndexedSet.hs to get a better idea of the workings of this library.

How does it work?
=================

An index is a simple datatype that contains a function `(a -> b)` and a `Map b (Set a)`. When you update the set by inserting a new item, the index function is run and the new item is added in the `Map`. This same principle applies to `delete`, `map` and `filter`.

This implies that you should *never* update the set without updating the indices. Fortunately, the library contains a template haskell function to derive methods that will update the set as well as the indices. Take a look at Main.hs for an example of this.

How fast is it?
===============

I don't know. The running times as compared to a regular `Data.Set` are indeed better, but I have not yet benchmarked it. It's probably not faster on relatively small data sets.