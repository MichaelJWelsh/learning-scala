package environments

import zio.console


object Addition2Example extends App {

  val runtime = zio.Runtime.unsafeFromLayer(console.Console.live >>> addition2.Addition.live)
  runtime.unsafeRun(addition2.Addition.addAndPrint(5))

}
