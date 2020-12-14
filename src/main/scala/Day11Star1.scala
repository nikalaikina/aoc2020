
import cats.implicits._

object Day11Star1 extends Main[Int] {

  override def File = "inputs/day11.txt"

  val Empty = 'L'
  val Floor = '.'
  val Occupied = '#'

  val steps = List(
    (0, 1),
    (1, 0),
    (1, 1),
    (-1, 1),
    (1, -1),
    (0, -1),
    (-1, 0),
    (-1, -1),
  )

  @scala.annotation.tailrec
  override def solve(grid: List[String]): List[Int] = {
    val lines = grid.length
    val columns = grid.head.length

    val step = (0 until lines).map { x =>
      val line = (0 until columns).map { y =>
        val current = grid(x)(y)
        if (current == Floor) {
          Floor
        } else {
          val adjacent = steps.flatMap { case (dx, dy) => grid.lift(x + dx).flatMap(_.lift(y + dy)) }
          val occupiedNear = adjacent.count(_ == Occupied)
          if (current == Occupied && occupiedNear > 3) {
            Empty
          } else if (current == Empty && occupiedNear == 0) {
            Occupied
          } else {
            current
          }
        }
      }
      line.mkString
    }.toList

    if (step == grid) {
      List(step.map(_.count(_ == Occupied)).sum)
    } else {
      solve(step)
    }
  }
}
