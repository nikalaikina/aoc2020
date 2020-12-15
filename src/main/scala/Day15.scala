
object Day15 extends Main[BigInt] {

  override def File = "inputs/day15.txt"

  val End = 30000000 // 2020

  override def solve(lines: List[String]): List[BigInt] = {
    val a = lines.head.split(",").map(_.toInt)

    val mem = a.zipWithIndex.toMap

    @scala.annotation.tailrec
    def rec(mem: Map[Int, Int], next: Int = 0, i: Int = a.length): Int = {
      if (i == End - 1) {
        next
      } else {
        rec(mem + (next -> i), mem.get(next).map(i - _).getOrElse(0), i + 1)
      }
    }

    List(rec(mem))
  }
}
