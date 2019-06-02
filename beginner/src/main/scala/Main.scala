object Main extends App {
  print("Please enter a number: ")

  val number = scala.io.StdIn.readInt()
  var total = 1

  for (x <- 1 to number)
    total = total * x

  println("Factorial of " + number.toString + " is " + total.toString)
}