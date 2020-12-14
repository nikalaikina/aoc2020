import scala.util.matching.Regex

object Day14Star2 extends Main[BigInt] {

  override def File = "inputs/day14.txt"

  lazy val assign: Regex = raw"mem\[(\d+)\] = (\d+)".r
  lazy val newMask: Regex = raw"mask = (\w+)".r

  lazy val EmptyMask = ("X" * 36).toList

  override def solve(lines: List[String]): List[BigInt] = {
    val (_, mem) = lines.foldLeft(EmptyMask, Map.empty[BigInt, Long]) {
      case ((mask, mem), assign(k, v)) =>
        val key = calc(k.toLong, mask)
        val news = key.map(_ -> v.toLong)
        (mask, mem ++ news)

      case ((_, mem), newMask(m)) =>
        (m.toList.reverse, mem)
    }
    List(mem.values.iterator.sum)
  }

  def calc(n: BigInt, m: List[Char], k: BigInt = 1): List[BigInt] = {
    m.headOption.map {
      case '0' => calc(n / 2, m.tail, k * 2).map(_ + (n % 2) * k)
      case '1' => calc(n / 2, m.tail, k * 2).map(_ + k)
      case 'X' => calc(n / 2, m.tail, k * 2).flatMap(x => List(x, x + k))
    }.getOrElse(List(BigInt(0)))
  }
}
