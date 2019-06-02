import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Main extends App {
  object Console {
    case class Print(msg: String)
    case object GetLine
    case class UserInput(input: String)
  }
  class ConsoleActor extends Actor {
    def receive: Receive = {
      case Console.Print(msg) => print(msg)
      case Console.GetLine => {
        val input = scala.io.StdIn.readLine()
        sender ! Console.UserInput(input)
      }
    }
  }

  object Validation {
    case class ValidateInt(str: String)
    case class ValidInt(i: Int)
    case object InvalidValue
  }
  class ValidationActor extends Actor {
    def receive: Receive = {
      case Validation.ValidateInt(n) =>
        try {
          sender ! Validation.ValidInt(n.toInt)
        } catch {
          case _: Exception => sender ! Validation.InvalidValue
        }
    }
  }

  object Calculation {
    case class CalculateFactorial(n: Int)
    case class FactorialEquals(n: Int, result: BigInt)
  }
  class CalculationActor extends Actor {
    var factorial: ActorRef =
      system.actorOf(Props[FactorialActor], name = "factorial")
    var parent: ActorRef = _
    var n: Int = _

    def receive: Receive = {
      case Calculation.CalculateFactorial(number) =>
        n = number
        parent = sender
        factorial ! Factorial.Of(n)
      case Factorial.Done(result) =>
        parent ! Calculation.FactorialEquals(n, result)
    }
  }

  object Factorial {
    case class Of(n: Int)
    case class Next(n: Int, total: BigInt)
    case class Done(result: BigInt)
  }
  class FactorialActor extends Actor {
    import context._
    var parent: ActorRef = _

    def recurse: Receive = {
      case Factorial.Next(n, total) =>
        if (n <= 1) sender ! Factorial.Done(total)
        else sender ! Factorial.Next(n - 1, total * n)
      case Factorial.Done(result) =>
        parent ! Factorial.Done(result)
    }

    def receive: Receive = {
      case Factorial.Of(n) =>
        parent = sender
        become(recurse)
        self ! Factorial.Next(n, 1)
    }
  }

  object Program {
    case class Run(consoleActor: ActorRef,
                   validationActor: ActorRef,
                   calculationActor: ActorRef)
  }
  class ProgramActor extends Actor {
    var console: ActorRef = _
    var validation: ActorRef = _
    var calculation: ActorRef = _

    def receive: Receive = {
      case Console.UserInput(input) =>
        validation ! Validation.ValidateInt(input)
      case Program.Run(consl, vld, calc) =>
        console = consl
        validation = vld
        calculation = calc

        console ! Console.Print("Please enter a number: ")
        console ! Console.GetLine
      case Validation.ValidInt(n) =>
        calculation ! Calculation.CalculateFactorial(n)
      case Validation.InvalidValue =>
        console ! Console.Print("Invalid number")
        context.system.terminate()
      case Calculation.FactorialEquals(n, result) =>
        console ! Console.Print(s"Factorial of $n is $result")
        context.system.terminate()
    }
  }

  val system = ActorSystem("FactorialSystem")

  val consoleActor = system.actorOf(Props[ConsoleActor], name = "console")
  val validationActor =
    system.actorOf(Props[ValidationActor], name = "validation")
  val calculationActor =
    system.actorOf(Props[CalculationActor], name = "calculation")
  val programActor = system.actorOf(Props[ProgramActor], name = "program")

  programActor ! Program.Run(consoleActor, validationActor, calculationActor)
}
