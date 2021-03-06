object Day1Star1 extends Main[Int] {

  def File = "inputs/day1.txt"

  override def solve(input: List[String]): List[Int] = {
    val ints = input.map(_.toInt)
    for {
      a <- ints
      b <- ints
      if a + b == `2020`
    } yield a * b
  }
}
