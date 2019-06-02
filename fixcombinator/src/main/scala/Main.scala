object Main extends App {

  def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }

  val gac: (=> BigInt => BigInt) => BigInt => BigInt =
    fac => n => if (n == 0) 1 else n * fac.apply(n - 1)

  val facF =
    fix[BigInt => BigInt](fac => n => if (n == 0) 1 else n * fac.apply(n - 1))

  val factorial = fix(gac)

  println(factorial(1000))
}
