object Main extends App {

  sealed trait Trampoline[+A] {
    def map[B](f: A => B): Trampoline[B] = flatMap(f.andThen(Done(_)))
    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = FlatMap(this, f)
  }

  final case class Done[A](a: A) extends Trampoline[A]
  final case class More[A](resume: () => Trampoline[A]) extends Trampoline[A]
  final case class FlatMap[A, B](sub: Trampoline[A], k: A => Trampoline[B])
      extends Trampoline[B]

  def run[A](trampoline: Trampoline[A]): A = trampoline match {
    case Done(a) => a
    case More(r) => run(r())
    case FlatMap(sub, cont) =>
      sub match {
        case Done(a)       => run(cont(a))
        case More(r)       => run(FlatMap(r(), cont))
        case FlatMap(sub2, cont2) => run(sub2.flatMap(cont2(_).flatMap(cont)))
      }
  }

  def factorial(n: Int): Trampoline[BigInt] =
    if (n == 0) Done(1)
    else More(() => factorial(n - 1)).flatMap(x => Done(n * x))

  val program = FlatMap(
    Done(print("Please enter a number: ")),
    (_: Unit) =>
      FlatMap(
        Done(scala.util.Try(scala.io.StdIn.readLine().toInt).toOption),
        (maybeNumber: Option[Int]) =>
          maybeNumber.fold(Done(println("Invalid number")): Trampoline[Unit])(
            n => factorial(n).map(fact => println(s"Factorial of $n is $fact")))
    )
  )

  run(program)
}
