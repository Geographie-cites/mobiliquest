package mobiliquest

object Mobiliquest extends App {

  val toulouse = CSV.read(io.Source.fromResource("BD_presence_Toulouse.csv").getLines())
  println("HEADERS " + toulouse.header.mkString(" | "))

  val h12 = toulouse.column("h12")
  println("H12 " + h12)
}