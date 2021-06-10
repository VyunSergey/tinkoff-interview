import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.control.NonFatal

object test2 extends App {
  /**
   *  На входе получаем последовательность асинхронных вызовов
   *  с сигнатурой Seq[Future[String]]
   *  Получить Future[(Seq[String], Seq[Throwable]) - результат агрегации
   *  выполненных Future и исключений
   */
  def sequenceF(seq: Seq[Future[String]])
               (implicit ec: ExecutionContext): Future[(Seq[String], Seq[Throwable])] = {
    Future.sequence(
      seq.map { ftr =>
        ftr.map(Right(_)).recover { case NonFatal(e) => Left(e) }
      }
    ).map { seq =>
      seq.foldLeft((Seq.empty[String], Seq.empty[Throwable])) {
        case ((res, errs), Left(e)) => (res, errs :+ e)
        case ((res, errs), Right(str)) => (res :+ str, errs)
      }
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  val (exp1, exp2, exp3) = (
    new RuntimeException("exception1"),
    new RuntimeException("exception2"),
    new RuntimeException("exception3")
  )
  val talk = Seq(
    Future {
      Thread.sleep(1000)
      "red"
    },
    Future.failed(exp1),
    Future.successful("blue"),
    Future.failed(exp2),
    Future.successful("green"),
    Future.failed(exp3)
  )

  val resF = sequenceF(talk)
  val (res, errs) = Await.result(resF, 1.minute)

  println(res)
  assert(res == Seq("red", "blue", "green"))

  println(errs)
  assert(errs == Seq(exp1, exp2, exp3))
}
