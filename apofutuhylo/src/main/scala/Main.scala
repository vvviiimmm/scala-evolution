object Main extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  final case class Fix[F[_]](unfix: F[Fix[F]])
  final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
  sealed trait Free[F[_], A]
  final case class Continue[F[_], A](a: A) extends Free[F, A]
  final case class Combine[F[_], A](fa: F[Free[F, A]]) extends Free[F, A]

  object Free {
    def continue[F[_], A](a: A): Free[F, A] = Continue(a)
    def combine[F[_], A](fa: F[Free[F, A]]): Free[F, A] = Combine(fa)
  }

  def ana[F[_], A](f: A => F[A])(a: A)(implicit F: Functor[F]): Fix[F] =
    Fix(F.map(f(a))(ana(f)))

  def cata[F[_], A](alg: F[A] => A)(e: Fix[F])(implicit F: Functor[F]): A =
    alg(F.map(e.unfix)(cata(alg)))

  def hylo[F[_], A, B](f: F[B] => B)(g: A => F[A])(a: A)(
      implicit F: Functor[F]): B =
    f(F.map(g(a))(hylo(f)(g)))

  def apo[F[_], A](f: A => F[Either[Fix[F], A]])(
      implicit F: Functor[F]): A => Fix[F] =
    a =>
      Fix(F.map(f(a)) {
        case Left(fix) => fix
        case Right(aa) => apo(f).apply(aa)
      })

  def futu[F[_], A](f: A => F[Free[F, A]])(
      implicit F: Functor[F]): A => Fix[F] = {
    def toFix: Free[F, A] => Fix[F] = {
      case Continue(a) => futu(f).apply(a)
      case Combine(fa) => Fix(F.map(fa)(toFix))
    }

    a =>
      Fix(F.map(f(a))(toFix))
  }

  sealed trait Stack[A]
  final case class Done[A](result: BigInt) extends Stack[A]
  final case class More[A](a: A, next: BigInt) extends Stack[A]

  object Stack {
    implicit val stackFunctor: Functor[Stack] = new Functor[Stack] {
      override def map[A, B](sa: Stack[A])(f: A => B): Stack[B] =
        sa match {
          case Done(result)  => Done(result)
          case More(a, next) => More(f(a), next)
        }
    }
  }

  object HyloFactorial {
    val stackCoalgebra: BigInt => Stack[BigInt] =
      n => if (n > 0) More(n - 1, n) else Done(1)

    val stackAlgebra: Stack[BigInt] => BigInt = {
      case Done(result)    => result
      case More(acc, next) => acc * next
    }

    def factorial: BigInt => BigInt = hylo(stackAlgebra)(stackCoalgebra)
  }

  object FutuFactorial {
    val F: Functor[Stack] = implicitly[Functor[Stack]]

    val stackCoalgebraFutu: BigInt => Stack[Free[Stack, BigInt]] =
      n =>
        if (n > 0)
          F.map(More(n - 1, n))(Free.continue)
        else F.map(Done(1))(Free.continue)

    def factorial: BigInt => BigInt =
      (futu(stackCoalgebraFutu) _).andThen(cata(HyloFactorial.stackAlgebra))
  }

  object ApoFactorial {
    val stackCoalgebraApo: BigInt => Stack[Either[Fix[Stack], BigInt]] =
      n => if (n > 0) More(Right(n - 1), n) else Done(1)

    def factorial: BigInt => BigInt =
      (apo(stackCoalgebraApo) _).andThen(cata(HyloFactorial.stackAlgebra))
  }

  println(HyloFactorial.factorial(1000))
  println(FutuFactorial.factorial(1000))
  println(ApoFactorial.factorial(1000))
}
