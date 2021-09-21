package mobiliquest

object Mobiliquest extends App {

  val dummy = CSV.read(io.Source.fromResource("dummy.csv").getLines())
  val third = dummy.column("third")

  println("THIRD COLUMN " + third)
}