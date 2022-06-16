package shared

import shared.data.RequestResponse

trait Api {

  def run(request: data.Request): RequestResponse
}