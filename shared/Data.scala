package shared


object Data {

  type Study = String
  type RequestID = String

  case class Request(study: Study)

}