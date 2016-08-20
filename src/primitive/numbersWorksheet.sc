

def intToRoman(x: Int):String = {

  val digits = List(1000 ->  "M", 900 -> "CM", 500 ->  "D",
    400 -> "CD", 100 ->  "C",  90 -> "XC",
    50 ->  "L",  40 -> "XL",  10 ->  "X",
    9 -> "IX",   5 ->  "V",   4 -> "IV",
    1 ->  "I")


  def intToRomanInternal(rem: Int,
                         remDigitList: List[(Int, String)],
                         acc: String = ""):String = rem match {
    case 0 => acc
    case _ => val filteredDigitList = remDigitList.dropWhile(_._1 > rem)
              intToRomanInternal(rem - filteredDigitList.head._1,
                filteredDigitList, acc ++ filteredDigitList.head._2)
  }

  intToRomanInternal(x, digits)
}




def intToRoman1(x: Int): String = {
  val digits = List(1000 ->  "M", 900 -> "CM", 500 ->  "D",
    400 -> "CD", 100 ->  "C",  90 -> "XC",
    50 ->  "L",  40 -> "XL",  10 ->  "X",
    9 -> "IX",   5 ->  "V",   4 -> "IV",
    1 ->  "I")

  def loop(l: List[(Int, String)], y: Int): String =
    if (y == 0) ""
    else l.head match {
      case (v, s) if (v <= y) => s + loop(l, y - v)
      case _ => loop(l.tail, y)
    }

  loop(digits, x)
}


val a = intToRoman(123123)
val b = intToRoman1(123123)
