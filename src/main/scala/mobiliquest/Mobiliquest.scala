package mobiliquest
import Quest._

object Mobiliquest extends App {

  val toulouse = CSV.read(io.Source.fromResource("BD_presence_Toulouse.csv").getLines())

  val request = toulouse
    .select("h12", "h13", "h14")
    .where("ZONAGE", SuchAs( _ == "3"))
    .where("KAGE", SuchAs( _ >= "3"))
    .quest

  println(request.size)
}