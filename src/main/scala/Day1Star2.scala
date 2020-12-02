object Day1Star2 extends Main[Int] {

  val File = "day1.txt"

  override def solve(input: List[String]): List[Int] = {
    val ints = input.map(_.toInt)
    for {
      a <- ints
      b <- ints
      c <- ints
      if a + b + c == `2020`
    } yield a * b * c
  }
}
