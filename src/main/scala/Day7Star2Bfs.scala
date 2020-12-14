
import cats.implicits._

object Day7Star2Bfs extends Main[Int] {

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

    def bfs(wave: Map[String, Int], count: Int = 0): Int = {
      wave.some.filter(_.nonEmpty).fold(count) { wave =>
        bfs(
          wave = wave.toList.foldMap { case (bag, n) =>
            grid.get(bag).orEmpty
              .foldMap { case (next, k) => Map(next -> n * k) }
          },
          count = count + wave.values.sum,
        )
      }
    }

    val res = bfs(Map(MyBag -> 1))
    List(res - 1)
  }
}
