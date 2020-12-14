
object Day10Star1 extends Main[Int] {

  override def File = "inputs/day10.txt"

  val Outlet = 0

  override def solve(input: List[String]): List[Int] = {
    val sorted = input.map(_.toInt).sorted
    val chain = Outlet :: (sorted :+ (sorted.last + 3))
    val zip = chain.init zip chain.tail
    val ones = zip.count { case (a, b) => b - a == 1 }
    val threes = zip.count { case (a, b) => b - a == 3 }
    List(ones * threes)
  }
}
