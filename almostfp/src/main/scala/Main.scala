import cats.effect.IO

import scala.util.{Failure, Success, Try}

object Main extends App {

  /**
    * Pure functions, we're good!
    */
  def putStr(msg: String): IO[Unit] = IO(print(msg))

  def getStr: IO[String] = IO(scala.io.StdIn.readLine())

  def validateInt(str: String): IO[Int] = IO(str.toInt)

  def factorial(n: Int): BigInt = Range.BigInt(1, n, 1).product

  /**
    * It's the end of the world now, no one cares.
    */
  putStr("Please enter a number: ").unsafeRunSync()
  val input = getStr.unsafeRunSync()
  Try(validateInt(input).unsafeRunSync()) match {
    case Success(n) =>
      putStr(s"Factorial of $n is ${factorial(n)}").unsafeRunSync()
    case Failure(_) => putStr("Invalid number").unsafeRunSync()
  }
}
