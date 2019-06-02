object Main extends App {

  trait Console[F[_]] {
    def putStr(msg: String): F[Unit]
    def getStr: F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  trait Calculator[F[_]] {
    def factorial(n: Int): F[BigInt]
  }

  object Calculator {
    def apply[F[_]](implicit F: Calculator[F]): Calculator[F] = F
  }

  trait Validation[F[_]] {
    def validateInt(str: String): F[Option[Int]]
  }

  object Validation {
    def apply[F[_]](implicit F: Validation[F]): Validation[F] = F
  }

  trait Monad[F[_]] {
    def pure[A](x: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  class IO[+A](val unsafeRun: () => A) { s =>
    def map[B](f: A => B): IO[B] = flatMap(f.andThen(IO(_)))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(f(s.unsafeRun()).unsafeRun())
  }
  object IO {
    def apply[A](eff: => A): IO[A] = new IO(() => eff)
  }

  implicit class MOps[M[_], A](m: M[A])(implicit monad: Monad[M]) {
    def flatMap[B](f: A => M[B]): M[B] = monad.flatMap(m)(f)
    def map[B](f: A => B): M[B] = monad.flatMap(m)(f.andThen(monad.pure))
  }

  def program[F[_]: Monad: Console: Calculator: Validation]: F[Unit] = {
    for {
      _ <- Console[F].putStr("Please enter a number: ")
      input <- Console[F].getStr
      maybeInt <- Validation[F].validateInt(input)
      _ <- maybeInt.fold(Console[F].putStr("Invalid number"))(
        n =>
          Calculator[F]
            .factorial(n)
            .flatMap(fact => Console[F].putStr(s"Factorial of $n is $fact")))
    } yield ()
  }

  implicit val consoleIO: Console[IO] = new Console[IO] {
    def putStr(msg: String): IO[Unit] = IO(print(msg))
    def getStr: IO[String] = IO(scala.io.StdIn.readLine())
  }

  implicit val validationIO: Validation[IO] = new Validation[IO] {
    import scala.util.Try
    def validateInt(str: String): IO[Option[Int]] =
      IO(Try(str.toInt).toOption)
  }

  implicit val calculatorIO: Calculator[IO] = new Calculator[IO] {
    def factorial(n: Int): IO[BigInt] = IO(Range.BigInt(1, n, 1).product)
  }

  implicit val ioMonad: Monad[IO] = new Monad[IO] {
    def pure[A](x: A): IO[A] = IO(x)
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
  }

  program[IO].unsafeRun()
}
