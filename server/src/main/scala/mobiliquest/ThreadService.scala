package mobiliquest

import shared.data.Request

import java.util.UUID
import java.util.concurrent._
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

object ThreadService {

  type Closure = () => Int

  class RequestClosure(closure: Closure) extends Callable[Int] {
    override def call: Int = {
      closure()
    }
  }

  implicit lazy val pool = Executors.newFixedThreadPool(5)

  implicit lazy val executionContext: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.fromExecutor(pool)


  def submit(closure: Closure) = Future[Int] {
    pool.submit(new RequestClosure(closure)).get
  }

}
