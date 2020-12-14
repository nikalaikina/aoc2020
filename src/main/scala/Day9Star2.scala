
object Day9Star2 extends Main[Long] {

  def File = "inputs/day9.txt"

  val Number = 15353384L

  override def solve(input: List[String]): List[Long] = {
    val a = input.map(_.toLong)

    @scala.annotation.tailrec
    def rec(l: Int = 0, r: Int = 0, sum: Long = a.head): Long = {
      if (sum == Number) {
        val range = a.slice(l, r)
        range.min + range.max
      } else if (sum < Number) {
        rec(l, r + 1, sum + a(r + 1))
      } else {
        rec(l + 1, r max l + 1, sum - a(l))
      }
    }
    List(rec())
  }
}
