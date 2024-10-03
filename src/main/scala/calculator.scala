import scala.util.boundary, boundary.break

@main def calculator(commands: String*): Int = {
  var acc: Int = 0
  var A: Int = 0
  var B: Int = 0
  var blink: Boolean = false

  /** Converts given string `s` to integer.
   *
   * Throws [[NumberFormatException]] if `s` can't be converted to integer,
   * but you shouldn't worry about it at this moment.
   */
  def parseInt(s: String): Int = s.toInt

  def writeIntoRegister(num: Int): Unit = {
    if (blink) {
      B = num
    } else {
      A = num
    }
    blink = !blink
  }

  def addition(): Unit = { acc = A + B }

  def difference(): Unit = { acc = A - B }

  def mult(): Unit = { acc = A * B }

  def div(): Unit = {
    if (B == 0) {
      A = 0
      B = 0
      acc = 0
    } else {
      acc = A / B
    }
  }

  def swap(): Unit = {
    val tmp = A
    A = B
    B = tmp
  }

  def blinkChange(): Unit = { blink = !blink }

  def accWriteToRegister(): Unit = {
    writeIntoRegister(acc)
  }

  boundary {
    for (c <- commands) {
      if (c.forall(Character.isDigit)) { // if c is number
        val num: Int = parseInt(c)
        writeIntoRegister(num)
      } else {
        c match {
          case "+" => addition()
          case "-" => difference()
          case "*" => mult()
          case "/" => div()
          case "swap" => swap()
          case "blink" => blinkChange()
          case "acc" => accWriteToRegister()
          case "break" => break()
          case _ => println("Error: Unknown command!")
        }
      }
    }
  }
  println(acc)
  acc
}
