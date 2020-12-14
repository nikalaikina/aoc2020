
object Day5Star2 extends Main[Int] {

  def File = "inputs/day5.txt"

  val F = 'F'
  val B = 'B'

  val L = 'L'
  val R = 'R'

  val Rows = 128
  val Columns = 8

  def binarySearch(conditions: List[Char], l: Int, r: Int): Int = {
    val half = (r - l) / 2
    conditions.headOption.fold(l) {
      case L => binarySearch(conditions.tail, l, r - half)
      case R => binarySearch(conditions.tail, l + half, r)
    }
  }

  override def solve(ps: List[String]): List[Int] = {
    val res = ps.map { boardingPass =>
      val (fb, lr) = boardingPass.replace(F, L).replace(B, R).splitAt(7)
      val row = binarySearch(fb.toList, 0, Rows)
      val column = binarySearch(lr.toList, 0, Columns)
      row * 8 + column
    }

    val sorted = res.sorted
    (sorted.init zip sorted.tail)
      .find { case (prev, next) => next - prev > 1 }
      .map { case (prev, _) => prev + 1 }
      .toList
  }
}


