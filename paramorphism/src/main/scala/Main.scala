object Main extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  sealed trait Nat[A]
  final case class Z[A]() extends Nat[A]
  final case class S[A](a: A) extends Nat[A]

  object Nat {
    implicit val natFunctor: Functor[Nat] = new Functor[Nat] {
      override def map[A, B](na: Nat[A])(f: A => B): Nat[B] =
        na match {
          case Z()  => Z()
          case S(a) => S(f(a))
        }
    }
  }

  final case class Fix[F[_]](unfix: F[Fix[F]])

  def ana[F[_], A](f: A => F[A])(a: A)(implicit F: Functor[F]): Fix[F] =
    Fix(F.map(f(a))(ana(f)))

  def cata[F[_], A](alg: F[A] => A)(e: Fix[F])(implicit F: Functor[F]): A =
    alg(F.map(e.unfix)(cata(alg)))

  def para[F[_], A](f: F[(Fix[F], A)] => A)(fix: Fix[F])(
      implicit F: Functor[F]): A =
    f(F.map(fix.unfix)(fix => (fix, para(f)(fix))))

  val natAlgebra: Nat[BigInt] => BigInt = {
    case Z()  => 1
    case S(n) => n + 1
  }

  val natAlgebraPara: Nat[(Fix[Nat], BigInt)] => BigInt = {
    case Z()           => 1
    case S((fix, acc)) => cata(natAlgebra)(fix) * acc
  }

  val natCoalgebra: BigInt => Nat[BigInt] =
    n => if (n == 0) Z() else S(n - 1)

  val factorial = (ana(natCoalgebra) _).andThen(para(natAlgebraPara))

  println(factorial(1000))
}
