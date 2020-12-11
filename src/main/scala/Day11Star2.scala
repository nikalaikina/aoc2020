
import cats._
import cats.implicits._

object Day11Star2 extends Main[Int] {

  override val File = "day11.txt"

  val Empty = 'L'
  val Floor = '.'
  val Occupied = '#'

  override def solve(grid: List[String]): List[Int] = {
    val rows = grid.length
    val columns = grid.head.length

    val firstColumn = grid.indices.toList.tupleRight(0)
    val lastColumn = grid.indices.toList.tupleRight(columns - 1)
    val firstRow = grid.head.indices.toList.tupleLeft(0)
    val lastRow = grid.head.indices.toList.tupleLeft(rows - 1)

    // line, column, starts
    val steps = List(
      (0, 1, firstColumn),
      (1, 0, firstRow),
      (1, 1, firstColumn ::: firstRow.tail),
      (0, -1, lastColumn),
      (-1, 0, lastRow),
      (-1, -1, lastColumn ::: lastRow.init),
      (1, -1, firstRow ::: lastColumn.tail),
      (-1, 1, firstColumn ::: lastRow.tail),
    )

    @scala.annotation.tailrec
    def rec(grid: List[String]): Int = {
      val array = Array.fill(rows)(Array.fill(columns)(0))

      for {
        (dx, dy, inits) <- steps
        (x, y) <- inits
      } ((x, y), 0).tailRecM[Id, Unit] {
        case ((x, y), acc) =>
          grid.lift(x).flatMap(_.lift(y)).map { current =>
            array(x)(y) += acc
            val next = current match {
              case Floor => acc
              case Occupied => 1
              case Empty => 0
            }
            ((x + dx, y + dy), next)
          }.toLeft(())
      }

      val step = (0 until rows).toList.map { x =>
        val line = (0 until columns).map { y =>
          val current = grid(x)(y)
          val occupiedNear = array(x)(y)
          current match {
            case Occupied if occupiedNear > 4 => Empty
            case Empty if occupiedNear == 0 => Occupied
            case other => other
          }
        }
        line.mkString
      }

      if (step == grid) {
        step.map(_.count(_ == Occupied)).sum
      } else {
        rec(step)
      }
    }

    List(rec(grid))
  }
}
