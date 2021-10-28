package mobiliquest
import Quest.*
import mobiliquest.CSV._
import java.io.{BufferedWriter, FileWriter}

object Mobiliquest extends App {

//  val lines = io.Source.fromFile("/home/mathieu/Bureau/BD_presence_Toulouse.csv").getLines()
//  val cleaned = lines.map {
//    _.split(";").mkString(",").replaceAll("\";\"", ",")
////      .map {
////      _.replaceAll("\";\"", ",").replaceAll("\"", "")
////    }.toSeq
//  }.toSeq
//
//
//  val out = new BufferedWriter(new FileWriter("/home/mathieu/work/cogit/mobiliquest/src/main/resources/BD_presence_Toulouse.csv"))
//
//  out.write(cleaned.mkString("\n"))
//  out.close


  println("READING ....")
  val toulouse = CSV.read(io.Source.fromResource("BD_presence_Toulouse.csv").getLines())
  //val toulouse = CSV.read(io.Source.fromResource("test.csv").getLines())

  println("REQUESTING ...")
  //toulouse.columns
    val request = toulouse
      .select("ID_IND", "h11", "CODE_SEC", "KAGE")
      .where("h11").isIn("TRUE")
      //.where("PAYS").isIn("FR")
      .where("CODE_SEC").isIn("002")
      .where("KAGE").isIn("3", "4")
      .where("SEX").isIn("2")
      .quest

//  val request = toulouse
//    .select("AGE")
//    .where("CODE_SEC").isIn("002")
//    .where("AGE").isIn("6","7")
//    .where("BB").isIn("6.5")
//    .quest

  //  println("COLUMNS " + request.headOption.map {_.length}.getOrElse(0))
  //  println("distinct " + request.transpose.head.distinct.size)
  //  request.foreach {println}


  println("REQUEST — COLS: " + request.columns.size + " | LINES: " + request.columns.headOption.map {
    _.size
  }.getOrElse(0))
}