val input = io.Source.fromFile("input.txt").getLines
val numbers = input.next.split(",").map(_.toInt)
val boards = input.filter(_.nonEmpty).map(_.trim).sliding(5, 5).toSeq.map(_.map(_.trim.split("\\s+").map(_.toInt)))

def bingo(numbers: Seq[Int])(b: Seq[Array[Int]]): Boolean =
  (b ++ b.transpose.map(_.toArray)).exists(r => r.forall(numbers.contains))

def unmarkedSum(numbers: Seq[Int])(b: Seq[Array[Int]]): Int = b.flatten.filterNot(numbers.contains).sum

def firstBoard(numbers: Seq[Int]): Option[Int] = boards.find(bingo(numbers)).map(unmarkedSum(numbers))

def lastBoard(numbers: Seq[Int]): Option[Int] =
  boards.filterNot(bingo(numbers.init)) match
    case Seq(last) if bingo(numbers)(last) => Some(unmarkedSum(numbers)(last))
    case _ => None

def result(sum: Seq[Int] => Option[Int]): Int =
  (1 to numbers.size)
    .to(LazyList)
    .flatMap(n => sum(numbers.take(n).toSeq).map(_ * numbers(n - 1)))
    .head

val (p1, p2) = (result(firstBoard), result(lastBoard))

def main(args: Array[String]): Unit =
  println(s"p1 = $p1")
  println(s"p2 = $p2")
