package shared

trait Api {

  def run(request: data.Request): String
}