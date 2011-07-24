Overview
========

IndexedSet provides fast querying on sets by providing indexing on arbitrary attributes.

Querying on indices is O(log n), rather than the O(n) performance you would get with a regular linear search on a set (i.e. Data.Set.filter). The downfall of this is that inserts into the set become a bit more expensive; it adds O(log n) for every insert operation.

See Main.hs for a simple example application that gives an overview of the basic functionality, and take a look at the source code in IndexedSet.hs to get a better idea of the workings of this library.

How does it work?
=================

A `SetIndex` is a simple datatype that contains a function `(a -> b)` and a `Map b (Set a)`. When you update the indexedset by inserting a new item, the index function is run and the new item is added in the `Map`. This same principle applies to `delete`, `map` and `filter`. This way, the indices are kept up to date with the data in the set.

This also implies that you should *never* update the set without updating the indices. Fortunately, the library contains template haskell functions to derive methods that will update the set as well as the indices. Take a look at Main.hs for an example of this.

How fast is it?
===============

I don't know. It completely depends on the situation. As a general rule, only use it when there are many more queries than inserts, because insertion time will grow with the amount of indices you add. A simple benchmark on 50k records is included in the file Benchmark.hs, and the results on my machine are that an index-based query takes approximately 93 microseconds while a `Data.Set.filter`-based query takes approximately 6.6 milliseconds.

Benchmarking smaller datasets also shows a distinct advantage of using indexedset, although the speed difference will get smaller with fewer elements.

Try it out in your own application to see if indexedset is right for your situation.

