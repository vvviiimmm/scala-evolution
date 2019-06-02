import org.apache.spark.{SparkConf, SparkContext}

import scala.util.{Failure, Success, Try}

object Main extends App {

  def createSparkContext: SparkContext = {
    val conf = new SparkConf().setMaster("local[*]").setAppName("Factorial")
    val sc = new SparkContext(conf)
    sc.setLogLevel("ERROR")
    sc
  }

  def factorial(sparkContext: SparkContext, num: BigInt): BigInt =
    if (num == 0) BigInt(1)
    else {
      val list = (BigInt(1) to num).toList
      sparkContext.parallelize(list).reduce(_ * _)
    }

  print("Please enter a number: ")
  Try(scala.io.StdIn.readLine().toInt) match {
    case Success(n) =>
      val context = createSparkContext
      val fact = factorial(context, n)
      println(s"Factorial of $n is $fact")
    case Failure(_) =>
      println("Invalid number")
  }
}
