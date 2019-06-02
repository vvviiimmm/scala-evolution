object Main extends App {

  sealed trait Nat
  case object Z extends Nat
  case class S(n: Nat) extends Nat

  def natToInt(nat: Nat): Int = nat match {
    case Z    => 0
    case S(n) => 1 + natToInt(n)
  }

  def intToNat(i: Int): Nat = i match {
    case 0 => Z
    case n => S(intToNat(n - 1))
  }

  def plus(a: Nat, b: Nat): Nat = a match {
    case Z    => b
    case S(n) => S(plus(n, b))
  }

  def mult(a: Nat, b: Nat): Nat = a match {
    case Z    => Z
    case S(n) => plus(b, mult(n, b))
  }

  def factorial(n: Nat): Nat = n match {
    case Z    => S(Z)
    case S(m) => mult(n, factorial(m))
  }

  print("Please enter a number: ")
  scala.util.Try(scala.io.StdIn.readLine().toInt).toOption
    .fold(println("Invalid number"))(n =>
      println(s"Factorial of $n is ${natToInt(factorial(intToNat(n)))}"))
}
