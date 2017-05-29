import scala.collection.mutable.ArrayBuffer

object Parser {
  private val getPrecedence = Map("+" -> 1, "-" -> 1, "*" -> 2, "/" -> 2, "^" -> 3).withDefaultValue(0)
  private val isRightAssociate = Map("^" -> true).withDefaultValue(false)
  private val isFunc = Map("abs" -> true, "sin" -> true, "cos" -> true, "tan" -> true, "sqrt" -> true).withDefaultValue(false)

  def isOperation(op: String) = getPrecedence(op) != 0
  def isFunction(func: String) = isFunc(func)

  private def isNumeric(str: String): Boolean = str.matches("[-+]?\\d+(\\.\\d+)?")

  private def tokenize(infix: String): ArrayBuffer[String] = {
    val tokens: ArrayBuffer[String] = ArrayBuffer()
    val str = infix.replace(" ", "")
    var nextType, start: Int = 0

    if (str.length() == 0) return tokens

    def getType(c: Char, prevType: Int): Int = {
      if (c.isDigit || c.equals('.')) return 1
      else if (isOperation(c.toString())) return 2
      else if (c.isLetter) return 3
      else if (prevType == 5) return 4 //when parentheses "))"
      return 5
    }

    var curType: Int = getType(str.charAt(0), 0)

    for (i <- 0 to str.length - 1) {
      if (i != str.length - 1) nextType = getType(str.charAt(i + 1), curType)
      else nextType = 0

      if (curType != nextType) {
        tokens.append(str.substring(start, i + 1))
        start = i + 1
      }

      curType = nextType
    }

    return tokens
  }

  //Shunting-yard: infix to postfix
  def parse(infix: String): ArrayBuffer[String] = {
    val stack: SimpleStack[String] = SimpleStack()
    val output: ArrayBuffer[String] = ArrayBuffer()
    val tokens: ArrayBuffer[String] = tokenize(infix)

    tokens.foreach {
      case (token: String) if (isNumeric(token)) => output.append(token)
      case (token: String) if (isFunc(token)) => stack.push(token)
      case (token: String) => getPrecedence(token) match {
        case 0 => {
          //not operation
          if (token == "(")
            stack.push(token)
          else if (token == ")") {
            while (!stack.isEmpty() && stack.peek() != "(")
              output.append(stack.pop())
            if (stack.isEmpty())
              throw new IllegalArgumentException("Mismatched parentheses")
            stack.pop()
            if (!stack.isEmpty() && isFunc(stack.peek()))
              output.append(stack.pop())
          }
          else throw new IllegalArgumentException("Unrecognized token " + token)
        }
        case (prec: Int) => {
          var break: Boolean = false
          var precNext: Int = 0
          while (!break && !stack.isEmpty() && {precNext = getPrecedence(stack.peek()); precNext != 0}) {
            if (prec < precNext || (prec == precNext && !isRightAssociate(token)))
              output.append(stack.pop())
            else break = true
          }
          stack.push(token)
        }
      }
    }

    while (!stack.isEmpty()) {
      stack.peek() match {
        case "(" | ")" => throw new IllegalArgumentException("Mismatched parentheses")
        case _ => output.append(stack.pop())
      }
    }

    return output
  }

}
