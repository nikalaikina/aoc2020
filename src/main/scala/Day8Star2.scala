
import cats.implicits._

object Day8Star2 extends Main[Int] {

  def File = "inputs/day8.txt"

  override def solve(input: List[String]): List[Int] = {
    val array = input.map { s =>
      val instr :: x :: Nil = s.split(" ").toList
      (instr, x.toInt)
    }.toArray

    def fix(acc: Int = 0, current: Int = 0, visited: Set[Int] = Set.empty, fixed: Boolean = false): Option[Int] = {
      if (visited(current)) {
        none[Int]
      } else if (current == array.length) {
        acc.some
      } else {
        array(current) match {
          case ("acc", d) =>
            fix(acc + d, current + 1, visited + current, fixed)
          case ("nop", _) if fixed =>
            fix(acc, current + 1, visited + current, fixed)
          case ("jmp", d) if fixed =>
            fix(acc, current + d, visited + current, fixed)
          case (i, d) if !fixed =>
            fix(acc, current + 1, visited + current, i != "nop") orElse
              fix(acc, current + d, visited + current, i != "jmp")
        }
      }
    }

    fix().toList
  }
}
