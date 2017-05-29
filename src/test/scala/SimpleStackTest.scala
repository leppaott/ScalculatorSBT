import org.scalatest._

class SimpleStackTest extends FlatSpec with Matchers {
  //thanks scalatest
  "SimpleStack" should "pop values in last-in-first-out order" in {
    val stack = new SimpleStack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be(2)
    stack.pop() should be(1)
  }

  it should "throw Exception if an empty stack is popped" in {
    val emptyStack = new SimpleStack[Int]
    a[Exception] should be thrownBy {
      emptyStack.pop()
    }
  }

  it should "allow peeking without removing" in {
    val stack = new SimpleStack[Int]
    stack.push(1)
    stack.push(2)
    stack.peek() should be(2)
    stack.pop() should be(2)
  }

  it should "include isEmpty-method" in {
    val stack = new SimpleStack[Int]
    stack.isEmpty() should be(true)
    stack.push(1)
    stack.isEmpty() should be(false)
  }
}
