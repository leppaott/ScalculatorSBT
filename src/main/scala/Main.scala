

object Main extends App {
  var line = readLine()
  do {
    try {
      println(Calculator.evaluate(line))
    } catch {
      case _ => println("Invalid statement");
    }
    line = readLine()
  } while (!line.isEmpty)
}