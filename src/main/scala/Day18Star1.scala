
object Day18Star1 extends Main[BigInt] {

  override def File = "inputs/day18.txt"

  override def solve(lines: List[String]): List[BigInt] = {

    def req(string: List[Char], stack: List[List[String]] = List(List())): Long = {
      string.headOption.fold(calc(stack.head)){
        case '(' =>
          req(string.tail, List.empty :: stack)
        case ')' =>
          val now :: prev :: tail = stack
          req(string.tail, (prev :+ calc(now).toString) :: tail)
        case x =>
          req(string.tail, (stack.head :+ x.toString) :: stack.tail)
      }
    }

    @scala.annotation.tailrec
    def calc(s: List[String]): Long = {
      s match {
        case last :: Nil => last.toString.toLong
        case a :: "+" :: b :: tail => calc((a.toString.toLong + b.toString.toLong).toString :: tail)
        case a :: "*" :: b :: tail => calc((a.toString.toLong * b.toString.toLong).toString :: tail)
      }
    }

    val res = lines.map { l =>
      req(l.replace(" ", "").toList)
    }

    List(res.iterator.sum)
  }

}
