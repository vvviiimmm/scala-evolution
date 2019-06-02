object Main extends App {

  trait Console {
    def printLine(msg: String): Unit
    def readLine: String
  }

  trait ConsoleComponent {
    val console: Console
  }

  trait ConsoleLive extends ConsoleComponent {
    lazy val console: Console = new Console {
      def printLine(msg: String): Unit = print(msg)
      def readLine: String = scala.io.StdIn.readLine()
    }
  }

  trait Validation {
    def validateInteger(str: String): Option[Int]
  }

  trait ValidationComponent {
    val validation: Validation
  }

  trait ValidationLive extends ValidationComponent {
    import scala.util.Try
    lazy val validation: Validation = new Validation {
      def validateInteger(str: String): Option[Int] = Try(str.toInt).toOption
    }
  }

  trait Calculator {
    def factorial(n: Int): BigInt
  }

  trait CalculatorComponent {
    val calculator: Calculator
  }

  trait CalculatorLive extends CalculatorComponent {
    lazy val calculator: Calculator = new Calculator {
      def factorial(n: Int): BigInt = {
        var total: BigInt = 1
        for (x <- 1 to n)
          total = total * x
        total
      }
    }
  }

  trait MyProgram {
    this: CalculatorComponent with ValidationComponent with ConsoleComponent =>

    def run(): Unit = {
      this.console.printLine("Please enter a number: ")
      val input = this.console.readLine
      val maybeNumber = this.validation.validateInteger(input)
      if (maybeNumber.isDefined) {
        val fact = this.calculator.factorial(maybeNumber.get)
        this.console.printLine(s"Factorial of ${maybeNumber.get} is $fact")
      } else
        console.printLine("Invalid number")
    }
  }

  val program = new MyProgram with CalculatorLive with ValidationLive with ConsoleLive

  program.run()
}
