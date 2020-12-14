
object Day6Star1 extends Main[Int] {

  def File = "inputs/day6.txt"

  override def solve(grid: List[String]): List[Int] = {
    val questions = grid.foldLeft(List(Set.empty[Char])) {
      case (acc, "") => Set.empty[Char] :: acc
      case (current :: tail, line) => (line.toSet ++ current) :: tail
    }
    List(questions.map(_.size).sum)
  }
}
