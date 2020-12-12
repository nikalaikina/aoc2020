
object Day12Star1 extends Main[Int] {

  override val File = "inputs/day12.txt"

  type Position = (Int, Int)

  val N = 'N'
  val S = 'S'
  val E = 'E'
  val W = 'W'
  val L = 'L'
  val R = 'R'
  val F = 'F'

  val East = (1, 0)
  val South = (0, -1)
  val West = (-1, 0)
  val North = (0, 1)

  val turns = List(East, South, West, North)

  override def solve(lines: List[String]): List[Int] = {
    val ((x, y), _) = lines.foldLeft((0, 0), 0: Int) { case (((x, y), facing), s) =>
      val steps = s.tail.toInt

      def go(delta: Position): Position = {
        val (dx, dy) = delta
        (x + dx * steps, y + dy * steps)
      }

      s.head match {
        case N => go(North) -> facing
        case S => go(South) -> facing
        case E => go(East) -> facing
        case W => go(West) -> facing
        case F => go(turns(facing)) -> facing
        case R => (x, y) -> (facing + steps / 90) % 4
        case L => (x, y) -> (facing - steps / 90 + 4) % 4
      }
    }

    List(math.abs(x) + math.abs(y))
  }
}
