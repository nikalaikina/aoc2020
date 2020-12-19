import scala.util.matching.Regex

object Day19Star1 extends Main[Int] {

  override def File = "inputs/day19.txt"
  
  type Correct = Map[Int, List[String]]

  lazy val or: Regex = raw"(\d+): ([\d\s]+) \| ([\d\s]+)".r
  lazy val last: Regex = raw"""(\d+): "(.)"""".r
  lazy val seq: Regex = raw"(\d+): (.+)".r

  override def solve(input: List[String]): List[Int] = {
    val (rulesLines, strings) = input.span(_.nonEmpty)

    val rules = rulesLines.map {
      case or(k, a, b) => k.toInt -> Or(a.toIntList, b.toIntList)
      case last(k, c) => k.toInt -> Last(c.head)
      case seq(k, s) => k.toInt -> Seq(s.toIntList)
    }.toMap

    def rec(calculated: Correct = Map.empty, k: Int = 0): Correct = {
      def chain(seq: List[Int]): Correct = seq.foldLeft(calculated)(rec)

      if (calculated.contains(k)) {
        calculated
      } else {
        rules(k) match {
          case Last(c) =>
            calculated + (k -> List(c.toString))
          case Or(l, r) =>
            val result = chain(l ::: r)
            result + (k -> (cartesian(l, result) ::: cartesian(r, result)))
          case Seq(l) =>
            val result = chain(l)
            result + (k -> cartesian(l, result))
        }
      }
    }

    val map = rec()
    val correct = map(0).toSet
    val ans = strings.tail.count(correct(_))

    List(ans)
  }

  def cartesian(seq: List[Int], calculated: Correct): List[String] = {
    seq.map(calculated).reduce(_ cross _)
  }

  implicit class Crossable(xs: List[String]) {
    def cross(ys: List[String]): List[String] = for {x <- xs; y <- ys} yield x ++ y
  }

  implicit class StringExt(s: String) {
    def toIntList: List[Int] = s.split(" ").map(_.toInt).toList
  }
}

sealed trait Rule
case class Last(c: Char) extends Rule
case class Or(a: List[Int], b: List[Int]) extends Rule
case class Seq(l: List[Int]) extends Rule
