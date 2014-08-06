package phase

class MultiPhasedProgressMock(phasesCount: Int) extends MultiPhasedProgress(phasesCount) {

  var movedProgresses = 0
  private var _phasesIns  = List[String]()
  private var _phasesOuts = List[String]()
  def phasesIns  = _phasesIns.reverse
  def phasesOuts = _phasesOuts.reverse

  override def moveProgress(phasesCount: Int) = {
    movedProgresses += phasesCount
    super.moveProgress(phasesCount)
  }

  override def inPhase[T](phaseName: String)(action: => T): T = {
    _phasesIns  = phaseName :: _phasesIns
    val result = super.inPhase(phaseName)(action)
    _phasesOuts = phaseName :: _phasesOuts
    result
  }

}
