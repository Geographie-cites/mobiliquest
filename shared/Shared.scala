package shared

import Data._

trait Api {

  def run(request: Data.Request): String
}