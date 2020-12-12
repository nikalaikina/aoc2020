import scala.math.abs

object Day12Star2 extends Main[Int] {

  override val File = "inputs/day12.txt"

  type Position = (Int, Int)

  val N = 'N'
  val S = 'S'
  val E = 'E'
  val W = 'W'
  val L = 'L'
  val R = 'R'
  val F = 'F'

  val Directions = Map(
    E -> (1, 0),
    S -> (0, -1),
    W -> (-1, 0),
    N -> (0, 1),
  )

  override def solve(lines: List[String]): List[Int] = {

    @scala.annotation.tailrec
    def rec(lines: List[String], ship: Position = (0, 0), waypoint: Position = (10, 1)): Position = {
      lines match {
        case Nil =>
          ship
        case s :: tail =>
          val k = s.tail.toInt
          rec(
            lines = tail,
            ship = s.head match {
              case F => ship go(waypoint, k)
              case _ => ship
            },
            waypoint = s.head match {
              case d if Directions.contains(d) => waypoint go(Directions(d), k)

              case R => waypoint turnR (k / 90)
              case L => waypoint turnL (k / 90)

              case _ => waypoint
            }
          )
      }
    }

    val (x, y) = rec(lines)
    List(abs(x) + abs(y))
  }

  implicit class PositionExt(p: Position) {
    val (x, y) = p

    def go(delta: Position, steps: Int): Position = {
      val (x, y) = p
      val (dx, dy) = delta
      (x + dx * steps, y + dy * steps)
    }

    def turnL: Position = (-y, x)

    def turnL(k: Int): Position = {
      if (k == 0) p else turnL(k - 1).turnL
    }

    def turnR(k: Int): Position = {
      turnL(4 - k)
    }
  }
}
