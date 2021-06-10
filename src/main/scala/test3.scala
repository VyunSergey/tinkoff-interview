object test3 extends App {
  /**
   *  Transformation Chain
   *  Дан набор возможных трансформаций: type Transformation[T] = T => Option[T]
   *  Написать функцию преобразования последовательности трансформаций в возможную трансформацию.
   *  Новая трансформация это результат работы всей цепочки трансформаций, которые не вернули None.
   *  Если все вернули None, то общий результат None.
   */

  type Transformation[T] = T => Option[T]

  def transformationChain[T](chain: Seq[Transformation[T]]): Transformation[T] =
    (t: T) => {
      chain.foldLeft(Option.empty[T]) { (acc, trm) =>
        acc.flatMap { t =>
          trm(t) match {
            case s: Some[T] => s
            case None => Some(t)
          }
        }.orElse(trm(t))
      }
    }

  val t1: Transformation[Int] = t => Some(t + t)
  val t2: Transformation[Int] = _ => None
  val t3: Transformation[Int] = t => if(t > 2) Some(t * t) else None

  val tc1 = transformationChain(Seq(t1,t2,t3))
  val tc2 = transformationChain(Seq(t2,t3))

  println(tc1(2), tc1(1), tc2(1))

  assert(tc1(2).contains(16))
  assert(tc1(1).contains(2))
  assert(tc2(1).isEmpty)
}
