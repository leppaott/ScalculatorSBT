import org.scalatest._
import org.scalatest.Assertions._

class ParserTest extends FlatSpec with Matchers {

  "Parser" should "return an empty buffer for empty input" in {
    assert(Parser.parse(" ").isEmpty)
  }

  it should "return infix input in postfix" in {
    assert(Parser.parse("6 + 5 * 4 / 3 + 5 * 2").mkString.equals("654*3/+52*+"))
    assert(Parser.parse("3 +5* 5 * (2/3+5)").mkString.equals("355*23/5+*+"))
    assert(Parser.parse("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3").mkString.equals("342*15-23^^/+"))
    assert(Parser.parse("sqrt(3 + 5 * sin(5 + 6 * 6))").mkString.equals("35566*+sin*+sqrt"))
    assert(Parser.parse("cos(tan(abs(3+3)))").mkString.equals("33+abstancos"))
  }

  it should "throw IllegalArgumentException if input contains mismatched parentheses" in {
    a[IllegalArgumentException] should be thrownBy {
      Parser.parse("2+3)")
    }
    a[IllegalArgumentException] should be thrownBy {
      Parser.parse("(2+3")
    }
    a[IllegalArgumentException] should be thrownBy {
      Parser.parse("2+3))")
    }
    a[IllegalArgumentException] should be thrownBy {
      Parser.parse("2)+3)")
    }
    a[IllegalArgumentException] should be thrownBy {
      Parser.parse("((2+3)")
    }
  }

  it should "throw IllegalArgumentException if input contains unrecognized tokens" in {
    a[IllegalArgumentException] should be thrownBy {
      Parser.parse("sin(2+3)*tang(2)")
    }
    a[IllegalArgumentException] should be thrownBy {
      Parser.parse("f(20*3)")
    }
    a[IllegalArgumentException] should be thrownBy {
      Parser.parse("x+3+3")
    }
    a[IllegalArgumentException] should be thrownBy {
      Parser.parse("40*x")
    }
  }

}
