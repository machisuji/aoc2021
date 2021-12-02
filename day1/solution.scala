def main(args: Array[String]): Unit = {
  val data: Seq[Int] = io.Source.fromFile("input.txt").getLines.filter(_.nonEmpty).map(_.toInt).toSeq
  def solution(xs: Seq[Int]): Int = xs.sliding(2, 1).count{case Seq(a, b) => b > a}

  val part1 = solution(data)
  val part2 = solution(data.sliding(3, 1).map(_.sum).toSeq)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}
