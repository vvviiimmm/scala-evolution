import cats.free.Free
import cats.~>
import freek.{:|:, DSL, NilDSL}

import freek._

object Main extends App {

  sealed trait Console[A]
  case class Print(msg: String) extends Console[Unit]
  case object ReadLine extends Console[String]

  sealed trait Validation[A]
  case class ValidateInteger(text: String) extends Validation[Option[Int]]

  sealed trait Calculation[A]
  case class Factorial(n: Int) extends Calculation[BigInt]

  /**
    * Program DSL is a coproduct of all the algebras
    */
  type PRG = Console :|: Validation :|: Calculation :|: NilDSL
  val PRG = DSL.Make[PRG]

  def program: Free[PRG.Cop, Unit] =
    for {
      _ <- Print("Please enter a number: ").freek[PRG]
      input <- ReadLine.freek[PRG]
      maybeNumber <- ValidateInteger(input).freek[PRG]
      _ <- maybeNumber.fold(Print("Invalid number").freek[PRG])(
        n =>
          Factorial(n)
            .freek[PRG]
            .flatMap(fact => Print(s"Factorial of $n is $fact").freek[PRG]))
    } yield ()

  type Id[A] = A

  /**
    * Interpreters for each algebra
    */
  val ConsoleInterpreter = new (Console ~> Id) {
    def apply[A](a: Console[A]): A = a match {
      case Print(msg) =>
        print(s"$msg")
      case ReadLine =>
        scala.io.StdIn.readLine()
    }
  }

  val ValidationInterpreter = new (Validation ~> Id) {
    import scala.util.Try
    def apply[A](a: Validation[A]): A = a match {
      case ValidateInteger(str) => Try(str.toInt).toOption
    }
  }

  val CalculationInterpreter = new (Calculation ~> Id) {
    def factorial(n: Int): BigInt = {
      def factorial_(n: Int, total: BigInt): BigInt =
        if (n <= 1)
          total
        else
          factorial_(n - 1, total * n)
      factorial_(n, 1)
    }
    def apply[A](a: Calculation[A]): A = a match {
      case Factorial(n) => factorial(n)
    }
  }

  /**
    * The program interpreter is a sum of all the interpreters
    */
  val interpreter = ConsoleInterpreter :&: ValidationInterpreter :&: CalculationInterpreter
  program.interpret(interpreter)
}
