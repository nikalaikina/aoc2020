
object Day9Star1 extends Main[Long] {

  def File = "inputs/day9.txt"

  val Preamble = 25

  override def solve(input: List[String]): List[Long] = {
    val a = input.map(_.toLong)
    val res = a.tails.find { list =>
      val (preamble, x :: _) = list.splitAt(Preamble)
      val sums = for {
        a <- preamble
        b <- preamble
        if a != b
      } yield a + b
      !sums.contains(x)
    }
    res.flatMap(_.drop(Preamble).headOption).toList
  }
}
