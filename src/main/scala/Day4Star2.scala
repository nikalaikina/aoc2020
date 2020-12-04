import scala.util.Try
import scala.util.matching.Regex

object Day4Star2 extends Main[Int] {

  val File = "day4.txt"

  val EyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  val HairColor: Regex = raw"#[\da-f]{6}".r

  val CmHeight: Regex = raw"(\d+)cm".r
  val InHeight: Regex = raw"(\d+)in".r

  val MandatoryFields: List[(String, String => Boolean)] = List(
    ("byr", s => Try(s.toInt).toOption.exists(yr => yr >= 1920 && yr <= 2002)),
    ("iyr", s => Try(s.toInt).toOption.exists(yr => yr >= 2010 && yr <= 2020)),
    ("eyr", s => Try(s.toInt).toOption.exists(yr => yr >= 2020 && yr <= 2030)),
    ("hgt", {
      case CmHeight(height) => Try(height.toInt).toOption.exists(h => h >= 150 && h <= 193)
      case InHeight(height) => Try(height.toInt).toOption.exists(h => h >= 59 && h <= 76)
      case _ => false
    }),
    ("hcl", HairColor.matches),
    ("ecl", EyeColors(_)),
    ("pid", s => s.length == 9 && s.forall(_.isDigit)),
  )

  override def solve(grid: List[String]): List[Int] = {
    val pasports = grid.foldLeft(List(List.empty[String])) {
      case (acc, "") => List.empty :: acc
      case (current :: tail, line) => (line.split(" ").toList ::: current) :: tail
    }
    val valid = pasports.count { fs =>
      val fnames = fs
        .map(_.split(":").toList)
        .map { case (k :: v :: _) => k -> v }
        .toMap
      MandatoryFields.forall { case (k, p) => fnames.get(k).exists(p) }
    }
    List(valid)
  }
}


