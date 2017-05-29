import scala.collection.mutable.ListBuffer

case class SimpleStack[A]() {
  private val stack: ListBuffer[A] = ListBuffer()

  def push(element: A): Unit = stack.prepend(element)

  def pop(): A = stack.remove(0)

  def peek(): A = stack.head

  def isEmpty(): Boolean = stack.isEmpty
}