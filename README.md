# collection-benchmark
Benchmarking suite for the Scala collections

Extracted from the benchmarking subproject of the [scala/collection-strawman](https://github.com/scala/collection-strawman) repository.  

There are four different benchmarks:
* `SeqBenchmark` benchmarks the collections extending `immutable.Seq`: `ArraySeq`, `List`, `LazyList` and `Vector`.
* `SetBenchmark` benchmarks the collections extending `immutable.Set`: `BitSet`, `HashSet`, `LongSet`, `ListSet` and `TreeSet`.
* `MapBenchmark` benchmarks the collections extending `immutable.Map`: `HashMap`, `LongMap` and `TreeMap`.
* `SeqMapBenchmark` benchmarks the collections extending `immutable.SeqMap`: `ListMap`, `OrderedMap` and `VectorMap`.

Run the time benchmarks using a command like the following (JMH flags can be specified, see sbt-jmh):
```sh
sbt benchmarks/jmh:run -rf json -t 1 -gc true .*
```
