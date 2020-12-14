
object Day6Star2 extends Main[Int] {

  def File = "inputs/day6.txt"
  val Alphabet = "abcdefghijklmnopqrstuvwxyz".toSet

  override def solve(grid: List[String]): List[Int] = {
    val questions = grid.foldLeft(List(Alphabet)) {
      case (acc, "") => Alphabet :: acc
      case (current :: tail, line) => (line.toSet & current) :: tail
    }
    List(questions.map(_.size).sum)
  }
}
