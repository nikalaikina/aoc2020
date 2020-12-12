
object Day3Star2 extends Main[Int] {

  val File = "inputs/day3.txt"

  val deltas = List(
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2),
  )

  val tree = '#'

  override def solve(grid: List[String]): List[Int] = {
    val res = deltas.map { case (dx, dy) =>
      def rec(x: Int = dx, y: Int = dy): Int = {
        grid.lift(y).fold(0) { line =>
          val trees = if (line(x % line.length) == tree) 1 else 0
          trees + rec(x + dx, y + dy)
        }
      }
      rec()
    }
    List(res.product)
  }
}
