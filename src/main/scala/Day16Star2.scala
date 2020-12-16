import scala.util.matching.Regex

object Day16Star2 extends Main[BigInt] {

  override def File = "inputs/day16.txt"

  lazy val clas: Regex =  raw"(\d+)-(\d+) or (\d+)-(\d+)".r

  lazy val N = 6

  override def solve(lines: List[String]): List[BigInt] = {
    val rulesLines = lines.takeWhile(_.nonEmpty)
    val rules = rulesLines
      .map(_.split(": ")(1))
      .map { case clas(a, b, c, d) =>
        ((a.toInt to b.toInt) ++ (c.toInt to d.toInt)).toSet
      }
    val correct = rules.flatten.toSet
    val valid = lines
      .drop(rulesLines.size + 5)
      .map(_.split(",").map(_.toInt).toList)
      .filter(_.forall(correct(_)))

    val mine = lines(rulesLines.size + 2)
      .split(",")
      .map(_.toInt)
      .toList

    val fields = valid.transpose.zipWithIndex

    val positions = rules.zipWithIndex
      .map { case (rule, ruleNumber) =>
        fields.collect { case (f, i) if f.forall(rule(_)) => i } -> ruleNumber
      }
      .sortBy(_._1.length)
      .foldLeft(Map.empty[Int, Int]) { case (found, (positions, fields)) =>
        found + (positions.find(!found.contains(_)).get -> fields)
      }
      .map(_.swap)

    val res = (0 until N)
      .map(positions)
      .map(mine)
      .map(_.toLong)
      .product

    List(res)
  }
}
