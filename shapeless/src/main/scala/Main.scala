object Main extends App {
  import shapeless._
  import nat._
  import ops.nat._

  trait Factorial[I <: Nat] { type Out <: Nat }

  object Factorial {
    def factorial[N <: Nat](i: Nat)(implicit fact: Factorial.Aux[i.N, N],
                                    wn: Witness.Aux[N]): N = wn.value

    type Aux[I <: Nat, Out0 <: Nat] = Factorial[I] { type Out = Out0 }

    implicit def fact0: Aux[_0, _1] = new Factorial[_0] { type Out = _1 }
    implicit def factN[N <: Nat, F <: Nat, F1 <: Nat](
        implicit f: Factorial.Aux[N, F1],
        t: Prod.Aux[Succ[N], F1, F]): Aux[Succ[N], F] =
      new Factorial[Succ[N]] { type Out = F }
  }

  val program = for {
    _ <- Lazy(print("Please enter a number: "))
    _ <- Lazy(scala.io.StdIn.readLine())
    _ <- Lazy(println(
      s"Well what's more important is that a factorial of 5 is ${toInt(Factorial
        .factorial(5))} and it was calculated at compile time, how cool is that?"))
  } yield ()

  program.value
}
