package mobiliquest

import java.util.concurrent._
import scala.concurrent.{ExecutionContext, Future}

object ThreadService {

  type Closure = () => Boolean

  class RequestClosure(closure: Closure) extends Callable[Boolean] {
    override def call: Boolean = {
      closure()
    }
  }

  implicit lazy val pool = Executors.newFixedThreadPool(5)

  implicit lazy val executionContext: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.fromExecutor(pool)


  def submit(closure: Closure) = Future[Boolean] {
    pool.submit(new RequestClosure(closure)).get
  }

}
