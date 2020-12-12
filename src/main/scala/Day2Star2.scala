import scala.util.matching.Regex

object Day2Star2 extends Main[Int] {

  val File = "inputs/day2.txt"

  val regex: Regex = raw"(\d+)-(\d+) (.): (.+)".r

  override def solve(input: List[String]): List[Int] = {
    val res = input.count {
      case regex(l, r, char, pass) =>
        pass(l.toInt - 1) == char.head ^ pass(r.toInt - 1) == char.head
    }
    List(res)
  }
}
