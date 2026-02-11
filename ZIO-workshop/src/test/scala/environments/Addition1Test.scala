package environments

import zio.test.Assertion.equalTo
import zio.test._

object Addition1Test extends DefaultRunnableSpec {

  def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Test Addition ZIO environment")(

    testM("using the +5 environment should respond with a ZIO effect that succeeds with 6 when we give it 1") {
      assertM(addition.Addition.addTo(1))(equalTo(6)).provideSomeLayer(addition.Addition.live1)
    },

    testM("using the +10 environment should respond with a ZIO effect that succeeds with 11 when we give it 1") {
      assertM(addition.Addition.addTo(1))(equalTo(11)).provideSomeLayer(addition.Addition.live2)
    }
  )

}
