
object Day3Star1 extends Main[Int] {

  def File = "inputs/day3.txt"

  val dx = 3
  val dy = 1

  val tree = '#'

  override def solve(grid: List[String]): List[Int] = {
    def rec(x: Int = dx, y: Int = dy): Int = {
      grid.lift(y).fold(0) { line =>
        val trees = if (line(x % line.length) == tree) 1 else 0
        trees + rec(x + dx, y + dy)
      }
    }
    List(rec())
  }
}
