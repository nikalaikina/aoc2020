
import cats.implicits._

object Day7Star1 extends Main[Int] {

  val File = "day7.txt"

  val MyBag = "shiny gold"

  override def solve(rules: List[String]): List[Int] = {
    val grid = rules.foldLeft(Map.empty[String, List[String]].withDefaultValue(List.empty)) {
      case (map, rule) =>
        val outer :: inner :: _ = rule.split(" bags contain ").toList
        val news = inner.init
          .split(", ")
          .map(_.split(" ").tail.init.mkString(" "))
          .map(_ -> List(outer))
        map |+| news.toMap
    }

    def dfs(bag: String, visited: Set[String]): Set[String] = {
      grid.get(bag).orEmpty.foldLeft(visited) {
        case (visited, bag) if visited contains bag =>
          visited
        case (visited, bag) =>
          dfs(bag, visited + bag)
      }
    }

    val res = dfs(MyBag, Set(MyBag))
    List(res.size - 1)
  }
}
