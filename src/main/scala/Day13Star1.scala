import scala.math.abs
import scala.util.Try

object Day13Star1 extends Main[Int] {

  override def File = "inputs/day13.txt"

  override def solve(lines: List[String]): List[Int] = {
    val fst :: snd :: Nil = lines
    val current = fst.toInt
    val buses = snd.split(",").toList.flatMap(b => Try(b.toInt).toOption)
    val res = buses.map { bus =>
      val last = (current / bus) * bus
      bus -> (last + bus - current)
    }
    res.foreach(println)
    val (bus, min) = res.minBy(_._2)
    List(bus * min)
  }

}
