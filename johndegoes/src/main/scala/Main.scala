import scalaz.zio.{DefaultRuntime, Fiber, Task, UIO, ZIO}

object Main extends App {

  object ConsoleIO {
    trait Service {
      def putStr(msg: String): Task[Unit]
      def getStr: Task[String]
    }
  }
  trait ConsoleIO {
    def consoleIO: ConsoleIO.Service
  }
  object consoleIO {
    def putStr(msg: String): ZIO[ConsoleIO, Throwable, Unit] =
      ZIO.accessM(_.consoleIO.putStr(msg))
    def getStr: ZIO[ConsoleIO, Throwable, String] =
      ZIO.accessM(_.consoleIO.getStr)
  }
  trait ConsoleIOLive extends ConsoleIO {
    def consoleIO: ConsoleIO.Service = new ConsoleIO.Service {
      def putStr(msg: String): Task[Unit] = Task.effect(print(msg))
      def getStr: Task[String] = Task.effect(scala.io.StdIn.readLine())
    }
  }
  object ConsoleIOLive extends ConsoleIOLive

  object Calculator {
    trait Service {
      def factorial(n: Int): UIO[BigInt]
    }
  }
  trait Calculator {
    def calculator: Calculator.Service
  }
  trait CalculatorLive extends Calculator {
    def calculator: Calculator.Service = new Calculator.Service {
      val numFibers = 10

      def factorial(n: Int): UIO[BigInt] =
        forkFibers(n, numFibers).foldLeft(UIO.succeed(BigInt(1)))(
          (totalIO, fiber) => totalIO.zipWith(fiber >>= (_.join))(_ * _))

      private def rangeFactorial(n: Long,
                                 lowerBound: Long,
                                 total: BigInt): BigInt =
        if (n <= lowerBound)
          total
        else
          rangeFactorial(n - 1, lowerBound, n * total)

      private def forkFibers(n: Int,
                             numFibers: Int): Seq[UIO[Fiber[Nothing, BigInt]]] =
        partialProductRanges(n, numFibers).map {
          case (h, l) => UIO(rangeFactorial(h, l, 1)).fork
        }

      def partialProductRanges(n: Int, fibers: Int): Seq[(Int, Int)] = {
        val numFibers = math.min(n, fibers)
        val step = scala.util.Try(n / numFibers).getOrElse(1)
        val remainder = scala.util.Try(n % numFibers).getOrElse(0)
        val genRange: Int => (Int, Int) = i => (i * step + step, i * step)
        if (remainder != 0)
          (0 until numFibers - 1)
            .map(genRange) :+ (n, n - remainder - step)
        else
          (0 until numFibers).map(genRange)
      }
    }
  }
  object CalculatorLive extends CalculatorLive

  object Validation {
    def validateInt(num: String): Option[Int] = scala.util.Try(num.toInt).toOption
  }

  val programZIO: ZIO[ConsoleIO with Calculator, Throwable, Unit] = for {
    _ <- consoleIO.putStr("Please enter a number: ")
    input <- consoleIO.getStr
    _ <- Validation
      .validateInt(input)
      .fold[ZIO[ConsoleIO with Calculator, Throwable, Unit]](
        consoleIO.putStr("Invalid number"))(number =>
        ZIO
          .accessM[Calculator](_.calculator.factorial(number)) >>= (factorial =>
          consoleIO.putStr(s"Factorial of $number is $factorial")))
  } yield ()

  object EnvironmentLive extends ConsoleIOLive with CalculatorLive

  val program = programZIO.provide(EnvironmentLive)

  val runtime = new DefaultRuntime {}

  runtime.unsafeRun(program)
}
