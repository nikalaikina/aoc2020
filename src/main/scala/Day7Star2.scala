
import cats.implicits._

object Day7Star2 extends Main[Int] {

  def File = "inputs/day7.txt"

  val MyBag = "shiny gold"

  override def solve(rules: List[String]): List[Int] = {
    val grid = rules.foldLeft(Map.empty[String, List[(String, Int)]].withDefaultValue(List.empty)) {
      case (map, rule) =>
        val outer :: inner :: _ = rule.split(" bags contain ").toList
        val news = inner.init
          .split(", ")
          .flatMap { s =>
            val n :: bag = s.split(" ").init.toList
            if (n == "no") {
              None
            } else {
              Some(bag.mkString(" ") -> n.toInt)
            }
          }
          .toList
          .foldMap(in => Map(outer -> List(in)))
        map |+| news
    }

    def dfs(current: String, visited: Map[String, Int], k: Int = 1): Map[String, Int] = {
      grid.get(current).orEmpty.foldLeft(visited) {
        case (visited, (bag, n)) =>
          dfs(bag, visited |+| Map(bag -> k * n), k * n)
      }
    }

    val res = dfs(MyBag, Map(MyBag -> 0))
    List(res.values.sum)
  }
}
