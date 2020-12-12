
object Day10Star2 extends Main[Long] {

  override val File = "inputs/day10.txt"

  override def solve(input: List[String]): List[Long] = {
    val numbers = input.map(_.toInt)
    val device = numbers.max + 3
    val exists = (device :: numbers).toSet

    val res = (1 to device)
      .foldLeft(List(0L, 0, 1)) { (three, jolts) =>
        val current = if (exists(jolts)) three.sum else 0
        three.tail :+ current
      }
      .last
    List(res)
  }
}
