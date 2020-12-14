import scala.util.matching.Regex

object Day14Star1 extends Main[BigInt] {

  override def File = "inputs/day14.txt"

  lazy val assign: Regex = raw"mem\[(\d+)\] = (\d+)".r
  lazy val newMask: Regex = raw"mask = (\w+)".r

  val EmptyMask = ("X" * 36).toList

  override def solve(lines: List[String]): List[BigInt] = {
    val (_, mem) = lines.foldLeft(EmptyMask, Map.empty[Int, BigInt]) {
      case ((mask, mem), assign(k, v)) =>
        val value = calc(v.toLong, mask)
        (mask, mem + (k.toInt -> value))
      case ((_, mem), newMask(m)) =>
        (m.toList.reverse, mem)
    }

    List(mem.values.iterator.sum)
  }

  def calc(n: BigInt, m: List[Char], k: BigInt = 1): BigInt = {
    if (m.isEmpty) {
      0L
    } else {
      val cur = m.head match {
        case 'X' => n % 2
        case '0' => BigInt(0)
        case '1' => BigInt(1)
      }
      (k * cur) + calc(n / 2, m.tail, k * 2)
    }
  }
}

//8566770985168
