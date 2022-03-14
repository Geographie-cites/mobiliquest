package shared

trait Api {


  def run(request: Data.Request): Data.RequestID
}