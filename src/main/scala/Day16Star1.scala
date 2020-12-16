import scala.util.matching.Regex

object Day16Star1 extends Main[BigInt] {

  override def File = "inputs/day16.txt"

  lazy val clas: Regex =  raw"(\d+)-(\d+) or (\d+)-(\d+)".r

  override def solve(lines: List[String]): List[BigInt] = {
    val rules = lines.takeWhile(_.nonEmpty)
    val correct = rules.map(_.split(": ")(1)).flatMap {
      case clas(a, b, c, d) => (a.toInt to b.toInt) ++ (c.toInt to d.toInt)
    }.toSet
    val res = lines
      .drop(rules.size + 5)
      .flatMap(_.split(","))
      .map(_.toInt)
      .filterNot(correct)
      .sum

    List(res)
  }
}
