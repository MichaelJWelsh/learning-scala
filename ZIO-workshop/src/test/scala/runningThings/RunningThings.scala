package runningThings

import creation.FromValues
import parallelism.Parallelism
import runningThings.RunningThings.runtime


object RunningThings extends App {

  val runtime: zio.Runtime[zio.ZEnv] = zio.Runtime.default
  //  println(runtime.unsafeRun(FromValues.zioBonnie))
  //
  //
  //  println(runtime.unsafeRun(Parallelism.numbersAsInts))
  //  println(runtime.unsafeRun(Parallelism.numbersAsIntsButQuick))

  //  runtime.unsafeRun(Parallelism.printNumbers)
  //  runtime.unsafeRun(Parallelism.printNumbersButQuick)

  println("slow max")
  println(runtime.unsafeRun(Parallelism.maxNumber))
  println("quick max")
  println(runtime.unsafeRun(Parallelism.maxNumberButQuick))

}
