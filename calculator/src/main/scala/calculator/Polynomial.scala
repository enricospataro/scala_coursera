package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val bValue = b()
    (bValue * bValue) - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    delta() match {
      case d if (d > 0) => Set(
        ((-b() + Math.sqrt(d)) / (2 * a())),
        ((-b() - Math.sqrt(d)) / (2 * a()))
      )
      case d if (d == 0) => Set(-b() / (2 * a()))
      case _ => Set()
    }
  }
}
