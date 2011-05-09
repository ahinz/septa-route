import org.scalatest._
import org.scalatest.matchers._

import org.hinz.septa._

class GISSpec extends Spec with ShouldMatchers {
  describe("GIS Utility") {
    it ("should be able to tell the distance between two latitude/longitude pts") {
      (GIS.distanceCalculator(
        24.466666666666665,
        54.36666666666667,
        5.55,
        0.21666666666666667)*100).toInt should equal(613860)
    }

    it("should create a valid bounds objects") {
      GIS.createBounds(1,2,40,50) should equal(Bounds(40,50,1,2))
      GIS.createBounds(40,50,1,2) should equal(Bounds(40,50,1,2))
    }

    it("should properly calculate min distances") {
      // Exact match
      GIS.minDistance((5,5),(1,0)) should equal(0)

      // Verified by hand :)
      (GIS.minDistance((10,20),(2,1))*1000).toInt should equal(745)
    }

    it("should compute lines correctly") {
      GIS.computeLine(0,0,5,5) should equal((1,0))
      GIS.computeLine(25,10,19,22) should equal((-2,60))
    }
  }
}
