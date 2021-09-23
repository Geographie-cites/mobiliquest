package mobiliquest
import Quest.*
import mobiliquest.CSV._

object Mobiliquest extends App {

  val toulouse = CSV.read(io.Source.fromResource("BD_presence_Toulouse.csv").getLines())

  val request = toulouse
    .select("h12", "h13", "h14")
    .where("h11").isIn("TRUE")
    .where("CODE_SEC").isIn("002")
    .where("KAGE").isIn("3","4")
    .quest

//  val toulouse = CSV.read(io.Source.fromResource("test.csv").getLines())
//
//  val request = toulouse
//    .select("AGE")
//    .where("CODE_SEC").isIn("002")
//    .where("AGE").suchAs((i: Int)=> i > 6)
//    .where("BB").suchAs((d: Double)=> d < 12.0)
//    .quest
  println("LINES " + request.size)
  println("COLUMNS " + request.headOption.map {_.length}.getOrElse(0))
}