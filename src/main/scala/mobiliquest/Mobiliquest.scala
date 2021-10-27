package mobiliquest

import Quest.*
import mobiliquest.CSV._

object Mobiliquest extends App {

  val toulouse = CSV.read(io.Source.fromResource("BD_presence_Toulouse.csv").getLines())
//  val toulouse = CSV.read(io.Source.fromResource("test.csv").getLines())

  //toulouse.columns
  val request = toulouse
    .select("ID_IND", "h11", "CODE_SEC", "KAGE")
    .where("h11").isIn("TRUE")
    //.where("PAYS").isIn("FR")
    .where("CODE_SEC").isIn("002")
    .where("KAGE").isIn("3", "4")
    .where("SEX").isIn("2")
    .quest

//    val request = toulouse
//      .select("AGE")
//      .where("CODE_SEC").isIn("002")
//      .where("AGE").isIn("6","7")
//      .where("BB").isIn("6.5")
//      .quest

  //  println("COLUMNS " + request.headOption.map {_.length}.getOrElse(0))
  //  println("distinct " + request.transpose.head.distinct.size)
  //  request.foreach {println}


  println("REQUEST — LINES: " + request.size + " | COLS: " + request.headOption.map{_.size}.getOrElse(0))
}