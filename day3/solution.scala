val data: Seq[String] = io.Source.fromFile("input.txt").getLines.toSeq

def digit(col: Int, gamma: Int = 1, xs: Seq[String] = data): Int =
  val n = xs
    .map(_.charAt(col).toString.toInt)
    .map(n => n - 1 * (1 - n))
    .sum

  if n == 0 then return -1

  if n > 0 then gamma else 0 + (1 - gamma)

def rate(gamma: Boolean): Int =
  val binary = (0 until data.head.size)
    .map(digit(_, if gamma then 1 else 0))
    .mkString

  Integer.valueOf(binary, 2)

def rating(oxygen: Boolean, col: Int = 0, inputs: Seq[String] = data): Int =
  val sign = digit(col, if oxygen then 1 else 0, inputs)

  if sign >= 0 then
    rating(oxygen, col + 1, inputs.filter(_.charAt(col).toString.toInt == sign))
  else
    val binary = inputs.filter(_.charAt(col) == (if oxygen then '1' else '0')).head

    Integer.valueOf(binary, 2)

def main(args: Array[String]): Unit =
  val (y, e) = (rate(true), rate(false))

  println(s"Solution 1 = $y * $e = ${y * e}")

  val (o2, co2) = (rating(true), rating(false))

  println(s"Solution 2 = $o2 * $co2 = ${o2 * co2}")
