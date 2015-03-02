scala-phases-chain
==================

Small lib for handling multi-phased processing in scala. You can build your chain of phases and after this you can run this chain with Progress runner. Progress can notify about state of operation e.g. akka actor or simply print it to logs.

# Examples

## Building phases chain

```scala
val chain =
  SinglePhasesChain("Preparation") { in: Int => PreparedData(in) } ::
  SinglePhasesChain("Validation") { data: PreparedData => ValidatedData(data) } ::
  SinglePhasesChain("Execution") { data: ValidatedData => Result(data) }
 
val runner = new ChainRunner(chain, progress)
val result = runner.run(123)
```

## Using akka actor
```scala
val service = new BackgroundService[Unit] {
  override def start(request: Unit, progressActor: ActorRef): Unit = {
    val runner = ChainRunner.actorBased(prepareChain, progressActor)
    runner.run()
  }
  (...)
}
val ref = system.actorOf(ProgressActor.props(service))

(...)

fixture.expectStatus(NotStarted)

fixture.startService()
fixture.expectStatus(InProgress("Preparing", 0))

fixture.addPhase(123)
fixture.addPhase(234)
fixture.addPhase(345)
fixture.expectStatus(InProgress("Preparing", 0))

fixture.executeOnServiceAndWaitAWhile(_.unlockPreparation())
fixture.expectStatus(InProgress("123", 25)) // preparing phase is also counted

fixture.executeOnServiceAndWaitAWhile(_.unlockPhase(123))
fixture.expectStatus(InProgress("234", 50))

fixture.executeOnServiceAndWaitAWhile(_.unlockPhase(234))
fixture.expectStatus(InProgress("345", 75))

fixture.executeOnServiceAndWaitAWhile(_.unlockPhase(345))
fixture.expectStatus(Completed(345))
```

# License

The scala-phases-chain is released under version 2.0 of the [Apache License](http://www.apache.org/licenses/LICENSE-2.0).

# More

For further reading take a look at blog post: http://blog.ingensol.pl/2014/08/multi-phased-processing-in-scala.html
