object test1 extends App {
  /**
   *  1) Сжать последовательность целых чисел
   *  Seq(1, 2, 2, 3, 4, 3, 3, 3) => Seq((1, 1), (2, 2), (3, 1), (4, 1), (3, 3))
   *  Ответ выдать в виде Seq[(Int, Int)] (число из последовательности и число последовательных повторений)
   *  2) восстановить исходную последовательность из сжатой
   */

  def collectP(seq: Seq[Int]): Seq[(Int, Int)] =
    seq.foldLeft(Seq.empty[(Int, Int)]) {
      case ((elm, cnt) :: tail, int) if elm == int =>
        (int, cnt + 1) +: tail
      case (acc, int) =>
        (int, 1) +: acc
      case (Nil, int) => Seq((int, 1))
    }

  def explodeP(seq: Seq[(Int, Int)]): Seq[Int] = {
    seq.flatMap { case (i, cnt) => Seq.fill(cnt)(i) }
  }

  val seq = Seq(1, 2, 2, 3, 4, 3, 3, 3)
  val res = collectP(seq).reverse
  val res2 = explodeP(res)

  println(res)
  assert(res == Seq((1, 1), (2, 2), (3, 1), (4, 1), (3, 3)))

  println(res2)
  assert(res2 == seq)
}
