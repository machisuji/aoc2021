def main(args: Array[String]): Unit = {
  val number = io.Source.fromFile("input.txt").getLines.filter(_.nonEmpty).map(_.toInt).sliding(3, 1).map(_.sum).sliding(2, 1).toSeq.count{case Seq(a, b) => b > a}
  
  println(number)
}
