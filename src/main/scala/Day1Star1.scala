object Day1Star1 extends Main[Int] {

  val File = "day1.txt"

  override def solve(input: List[String]): List[Int] = {
    val ints = input.map(Integer.parseInt)
    for {
      a <- ints
      b <- ints
      if a + b == `2020`
    } yield a * b
  }
}
