
object Day4Star1 extends Main[Int] {

  def File = "inputs/day4.txt"

  lazy val MandatoryFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  override def solve(grid: List[String]): List[Int] = {
    val pasports = grid.foldLeft(List(List.empty[String])) {
      case (acc, "") => List.empty :: acc
      case (current :: tail, line) => (line.split(" ").toList ::: current) :: tail
    }
    val valid = pasports.count { fs =>
      val fnames = fs.flatMap(_.split(":").headOption)
      MandatoryFields.forall(fnames.contains)
    }
    List(valid)
  }
}
