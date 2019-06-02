object Main extends App {

  case class Coproduct[F[_], G[_], A](run: Either[F[A], G[A]])

  trait Monad[F[_]] {
    def pure[A](x: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  object Monad {
    def apply[F[_]](implicit monad: Monad[F]): Monad[F] = monad
  }

  type Id[A] = A

  implicit val monadId: Monad[Id] = new Monad[Id] {
    def pure[A](x: A): Id[A] = x
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  trait NaturalTransformation[F[_], G[_]] { self =>
    def transform[A](fa: F[A]): G[A]

    def or[H[_]](f: NaturalTransformation[H, G])
      : NaturalTransformation[({ type f[x] = Coproduct[F, H, x] })#f, G] =
      new NaturalTransformation[({ type f[x] = Coproduct[F, H, x] })#f, G] {
        def transform[A](c: Coproduct[F, H, A]): G[A] = c.run match {
          case Left(fa)  => self.transform(fa)
          case Right(ha) => f.transform(ha)
        }
      }
  }

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a)          => f(a)
      case FlatMap(sub, cont) => FlatMap(sub, cont andThen (_ flatMap f))
    }
    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
    def foldMap[G[_]: Monad](f: NaturalTransformation[F, G]): G[A] =
      this match {
        case Return(a) => Monad[G].pure(a)
        case FlatMap(fx, g) =>
          Monad[G].flatMap(f.transform(fx)) { a =>
            g(a).foldMap(f)
          }
      }
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A])
      extends Free[F, A]

  object Free {
    implicit def liftF[F[_], A](fa: F[A]): Free[F, A] =
      FlatMap(fa, Return.apply)

    def runFree[F[_], G[_], A](
        prg: Free[F, A],
        nt: NaturalTransformation[F, G])(implicit M: Monad[G]): G[A] =
      prg match {
        case Return(a) => Monad[G].pure(a)
        case FlatMap(sub, cont) => {
          val transformed = nt.transform(sub)
          Monad[G].flatMap(transformed) { a =>
            runFree(cont(a), nt)
          }
        }
      }
  }

  sealed trait Inject[F[_], G[_]] {
    def inj[A](sub: F[A]): G[A]
    def prj[A](sup: G[A]): Option[F[A]]
  }

  object Inject {
    implicit def injRefl[F[_]]: Inject[F, F] = new Inject[F, F] {
      def inj[A](sub: F[A]): F[A] = sub
      def prj[A](sup: F[A]) = Some(sup)
    }

    implicit def injLeft[F[_], G[_]] =
      new Inject[F, ({ type λ[α] = Coproduct[F, G, α] })#λ] {
        def inj[A](sub: F[A]) = Coproduct(Left(sub))
        def prj[A](sup: Coproduct[F, G, A]): Option[F[A]] = sup.run match {
          case Left(fa) => Some(fa)
          case Right(_) => None
        }
      }

    implicit def injRight[F[_], G[_], H[_]](implicit I: Inject[F, G]) =
      new Inject[F, ({ type f[x] = Coproduct[H, G, x] })#f] {
        def inj[A](sub: F[A]) = Coproduct(Right(I.inj(sub)))
        def prj[A](sup: Coproduct[H, G, A]): Option[F[A]] = sup.run match {
          case Left(_)  => None
          case Right(x) => I.prj(x)
        }
      }
  }

  def lift[F[_], G[_], A](f: F[A])(implicit I: Inject[F, G]): Free[G, A] =
    FlatMap(I.inj(f), Return(_: A))

  sealed trait Console[A]

  case class Print(msg: String) extends Console[Unit]

  case object ReadLine extends Console[String]

  sealed trait Validation[A]

  case class ValidateInteger(text: String) extends Validation[Option[Int]]

  sealed trait Calculation[A]
  case class Factorial(n: Int) extends Calculation[BigInt]

  class HasConsole[F[_]](implicit I: Inject[Console, F]) {
    def putStr(msg: String): Free[F, Unit] = lift(Print(msg))
    def getStr: Free[F, String] = lift(ReadLine)
  }

  class HasValidation[F[_]](implicit I: Inject[Validation, F]) {
    def validateInteger(str: String): Free[F, Option[Int]] =
      lift(ValidateInteger(str))
  }

  object HasConsole {
    implicit def instance[F[_]](implicit I: Inject[Console, F]): HasConsole[F] =
      new HasConsole[F]
  }

  object HasValidation {
    implicit def instance[F[_]](
        implicit I: Inject[Validation, F]): HasValidation[F] =
      new HasValidation[F]
  }

  val consoleInterpreter: NaturalTransformation[Console, Id] =
    new NaturalTransformation[Console, Id] {
      def transform[A](fa: Console[A]): Id[A] = fa match {
        case Print(str) =>
          print(str)
        case ReadLine =>
          scala.io.StdIn.readLine()
      }
    }

  val validationInterpreter: NaturalTransformation[Validation, Id] =
    new NaturalTransformation[Validation, Id] {
      import scala.util.Try
      def transform[A](fa: Validation[A]): Id[A] = fa match {
        case ValidateInteger(str) => Try(str.toInt).toOption
      }
    }

  /**
    * Alright, now let's actually write our program
    */
  def freeProgram[F[_]](implicit C: HasConsole[F],
                        V: HasValidation[F]): Free[F, Unit] = {
    def factorial(n: Int): BigInt = Range.BigInt(1, n, 1).product
    for {
      _ <- C.putStr("Please enter a number: ")
      input <- C.getStr
      maybeNumber <- V.validateInteger(input)
      _ <- maybeNumber.fold(C.putStr("Invalid number"))(n =>
        C.putStr(s"Factorial of $n is ${factorial(n)}"))
    } yield ()
  }

  val programInterpreter = consoleInterpreter or validationInterpreter

  type AppDSL[A] = Coproduct[Console, Validation, A]

  /**
    * Instantiate the program for AppDSL
    */
  val app: Free[AppDSL, Unit] = freeProgram[AppDSL]

  app.foldMap(programInterpreter)
}
