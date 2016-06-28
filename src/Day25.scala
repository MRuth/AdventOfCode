import java.nio.channels.Pipe.SourceChannel

import scala.io.Source

/**
  * Created by Montana Ruth on 6/28/2016.
  */

object Day25 extends App {
  val regexPattern = """.+row (\d+), column (\d+).+""".r
  val in = Source.fromFile("Files/Day25.in").getLines().next()


  val matcher = regexPattern.findAllMatchIn(in).next()
  val targetRow = matcher.group(1).toInt
  val targetCol = matcher.group(2).toInt

  val firstNumInCol = (2 to targetCol).foldLeft(1)((prev,i) => prev+i)
  val index: Long = (0 to targetRow-2).foldLeft(firstNumInCol)((prev,i) => prev+targetCol+i)
  val code: Long = (1L until index).foldLeft(20151125L)((prev,_) => (prev*252533L)  %33554393L)


  println(s"Part 1: $code")
}
