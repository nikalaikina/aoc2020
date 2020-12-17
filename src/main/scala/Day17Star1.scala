import scala.util.matching.Regex

object Day17Star1 extends Main[BigInt] {

  override def File = "inputs/day17.txt"

  lazy val Active = '#'

  type Coordinate = (Int, Int, Int)

  lazy val Steps = {
    for {
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      if !(x == 0 && y == 0 && z == 0)
    } yield (x, y, z)
  }

  override def solve(lines: List[String]): List[BigInt] = {

    val active = lines.zipWithIndex.flatMap { case (line, i) =>
      line.toList.zipWithIndex.collect { case (Active, j) => (i, j, 0) }
    }

    @scala.annotation.tailrec
    def rec(xx: Axe, yy: Axe, zz: Axe, active: Set[Coordinate], k: Int = 6): Int = {
      if (k == 0) {
        active.size
      } else {
        val newActive = for {
          x <- xx.range
          y <- yy.range
          z <- zz.range
          k = Steps.count { case (dx, dy, dz) =>
            active((x + dx, y + dy, z + dz))
          }
          becomesActive = if (active((x, y, z))) {
            k == 3 || k == 2
          } else {
            k == 3
          }
          if becomesActive
        } yield (x, y, z)
        rec(xx.widen, yy.widen, zz.widen, newActive.toSet, k - 1)
      }
    }

    List(rec(
      xx = Axe(active.map(_._1)),
      yy = Axe(active.map(_._2)),
      zz = Axe(0, 0).widen,
      active.toSet,
    ))
  }

}

case class Axe(l: Int, r: Int) {
  def widen: Axe = Axe(l - 1, r + 1)
  def range: Range.Inclusive = l to r
}

object Axe {
  def apply(l: List[Int]): Axe = Axe(l.min, l.max).widen
}
