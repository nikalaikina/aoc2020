
object Day8Star1 extends Main[Int] {

  def File = "inputs/day8.txt"

  override def solve(input: List[String]): List[Int] = {
    val array = input.map { s =>
      val instr :: x :: Nil = s.split(" ").toList
      (instr, x.toInt)
    }.toArray

    @scala.annotation.tailrec
    def rec(acc: Int = 0, current: Int = 0, visited: Set[Int] = Set.empty): Int = {
      if (visited(current)) {
        acc
      } else {
        array(current) match {
          case ("nop", _) => rec(acc, current + 1, visited + current)
          case ("acc", d) => rec(acc + d, current + 1, visited + current)
          case ("jmp", d) => rec(acc, current + d, visited + current)
        }
      }
    }

    List(rec())
  }
}
