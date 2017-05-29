import scala.collection.mutable.ArrayBuffer

object Calculator {
  private val op = Map("+" -> ((x: Double, y: Double) => x + y), "-" -> ((x: Double, y: Double) => x - y),
    "*" -> ((x: Double, y: Double) => x * y), "/" -> ((x: Double, y: Double) => x / y), "^" -> ((x: Double, y: Double) => Math.pow(x, y)))
  private val func = Map("abs" -> (Math.abs(_: Double)), "sin" -> (Math.sin(_: Double)), "cos" -> (Math.cos(_: Double)),
    "tan" -> (Math.tan(_: Double)), "sqrt" -> (Math.sqrt(_: Double)))

  private val isOp: (String => Boolean) = Parser.isOperation(_)
  private val isFunc: (String => Boolean) = Parser.isFunction(_)

  def evaluate(infix: String): String = {
    val stack: SimpleStack[String] = SimpleStack()
    val rpn: ArrayBuffer[String] = Parser.parse(infix)
    println(rpn)
    def handleToken(token: String): Unit = {
      if (isOp(token)) {
        val right: Double = stack.pop().toDouble
        val left: Double = stack.pop().toDouble
        stack.push(op(token)(left, right).toString)
      } else if (isFunc(token)) {
        stack.push(func(token)(stack.pop().toDouble).toString)
      } else {
        stack.push(token) //number
      }
    }

    rpn.foreach(handleToken)

    val res = stack.pop().toDouble
    if (res.floor == res.ceil)
      return res.toInt.toString

    res.toString
  }
}
