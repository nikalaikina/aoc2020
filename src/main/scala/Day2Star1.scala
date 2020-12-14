import scala.util.matching.Regex

object Day2Star1 extends Main[Int] {

  def File = "inputs/day2.txt"

  val regex: Regex = raw"(\d+)-(\d+) (.): (.+)".r

  override def solve(input: List[String]): List[Int] = {
    val res = input.count {
      case regex(from, to, char, pass) =>
        println(1)
        val c = pass.count(_ == char.head)
        c >= from.toInt && c <= to.toInt
    }
    List(res)
  }
}
