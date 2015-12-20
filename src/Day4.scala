import java.security.MessageDigest


/**
  * Created by Montana Ruth on 12/18/2015.
  */

object Day4 extends App {

  val key = "bgvyzdsv"

  def isAdventCoin(s: String,leadingZeros: Int): Boolean = md5Hash(s).take(leadingZeros).mkString == "0" * leadingZeros

  def md5Hash(s: String) : String = MessageDigest.getInstance("MD5").digest(s.getBytes("UTF-8")).map(0xFF & _).map(i => f"$i%02x").mkString


  def part_1(): String = {
    val leadingZeros =  5

    (Stream.from(1).takeWhile(i => !isAdventCoin(key + i,leadingZeros)).last + 1).toString
}

  def part_2(): String = {
    val leadingZeros = 6
    (Stream.from(254575).takeWhile(i => !isAdventCoin(key + i,leadingZeros)).last + 1).toString
  }

  println(s"Part 1: ${part_1}")
  println(s"Part 2: ${part_2}")

}
