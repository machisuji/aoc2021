def main(args: Array[String]): Unit =
  val data: Seq[(String, Int)] = io.Source.fromFile("input.txt")
    .getLines
    .map(_.split(" "))
    .map{ case Array(dir, dist) => (dir, dist.toInt) }
    .toSeq
  
  val (x1, y1) = data.foldLeft((0, 0)) {
    case ((x, y), (dir, dist)) =>
      dir match
        case "forward" => (x + dist, y)
        case "up" => (x, y - dist)
        case "down" => (x, y + dist)
  }

  println(s"Solution 1 = ($x1, $y1) = ${x1 * y1}")

  val (x2, y2, _) = data.foldLeft((0, 0, 0)) {
    case ((x, y, aim), (dir, dist)) =>
      dir match
        case "forward" => (x + dist, y + aim * dist, aim)
        case "up" => (x, y, aim - dist)
        case "down" => (x, y, aim + dist)
  }

  println(s"Solution 2 = ($x2, $y2) = ${x2 * y2}")
