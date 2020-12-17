
object Day17Star2 extends Main[BigInt] {

  override def File = "inputs/day17.txt"

  lazy val Active = '#'

  type Coordinate = (Int, Int, Int, Int)

  lazy val Steps = {
    for {
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      t <- -1 to 1
      if !(x == 0 && y == 0 && z == 0 && t == 0)
    } yield (x, y, z, t)
  }

  override def solve(lines: List[String]): List[BigInt] = {

    val active = lines.zipWithIndex.flatMap { case (line, i) =>
      line.toList.zipWithIndex.collect { case (Active, j) => (i, j, 0, 0) }
    }

    @scala.annotation.tailrec
    def rec(xx: Axe, yy: Axe, zz: Axe, tt: Axe, active: Set[Coordinate], k: Int = 6): Int = {
      if (k == 0) {
        active.size
      } else {
        val newActive = for {
          x <- xx.range
          y <- yy.range
          z <- zz.range
          t <- tt.range
          k = Steps.count { case (dx, dy, dz, dt) =>
            active((x + dx, y + dy, z + dz, t + dt))
          }
          becomesActive = if (active((x, y, z, t))) {
            k == 3 || k == 2
          } else {
            k == 3
          }
          if becomesActive
        } yield (x, y, z, t)
        rec(xx.widen, yy.widen, zz.widen, tt.widen, newActive.toSet, k - 1)
      }
    }

    List(rec(
      xx = Axe(active.map(_._1)),
      yy = Axe(active.map(_._2)),
      zz = Axe(0, 0).widen,
      tt = Axe(0, 0).widen,
      active.toSet,
    ))
  }

}

