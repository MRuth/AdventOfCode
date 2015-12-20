import scala.io.Source

/**
  * Created by Montana Ruth on 12/20/2015.
  */

object Day8_test extends App {

  def unescape(input: String): String = {
    val it = input.substring(1, input.length - 1).iterator
    val out = new StringBuilder
    while(it.hasNext) {
      val c = it.next()

      if(c == '\\') {
        val c2 = it.next()
        if(c2 == '"' || c2 == '\\') {
          out.append(c2)
        }
        else if(c2 == 'x') {
          val v = Integer.parseInt(it.next().toString + it.next.toString, 16)
          out.append(v.toChar)
        }
      }
      else {
        out.append(c)
      }
    }

    out.toString()
  }

  val in = Source.fromFile("Files/Day8.in").getLines().toSeq
  val totalChars = in.map(_.length).sum
  val totalParsedChars = in.map(unescape).map(_.length).foreach(println)
}
