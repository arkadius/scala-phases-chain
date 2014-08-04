scala-phases-chain
==================

Small lib for handling multi-phased processing in scala. You can build your chain of phases and after this you can run this chain with Progress runner. Progress can notify about state of operation e.g. akka actor or simply print it to logs.

Example
=======

```scala
val chain =
  Phase("Preparation") { in: Int => PreparedData(in) } ::
  Phase("Validation") { data: PreparedData => ValidatedData(data) } ::
  Phase("Execution") { data: ValidatedData => Result(data) } ::
  NilChain()
 
val progress = new MultiPhasedProgress(chain)
val result = progress.run(123)
```

More
====

For future reading take a look at blog post: http://blog.ingensol.pl/2014/08/multi-phased-processing-in-scala.html
