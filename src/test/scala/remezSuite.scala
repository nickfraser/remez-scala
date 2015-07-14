
import Remez._
import breeze.linalg._
import org.scalatest._

/** A test suite for the Remez class.
  *
  * Currently tests sin(x) and exp(x).
  */
class RemezSuite extends FlatSpec with Matchers {

    // Test 1.
    val exp1 = DenseVector(-4.850637758584954e-12, 1.000000089406958e+00, -2.441406092672303e-04)
    "The Remez class" should "find the coeffs for test 2 (params: f(x)=sin(x), low=0.0, hi=2^-10, order=2)" in {
        val tes1 = new Remez(math.sin(_), math.cos(_), 0.0, math.pow(2,-10), 2)
        tes1.coeffs.toArray should be (exp1.toArray)
    }

    // Test 2.
    val exp2 = DenseVector(1.000000000004853e+00, 9.999999105564560e-01, 5.002442127611959e-01)
    "The Remez class" should "find the coeffs for test 1 (params: f(x)=exp(x), low=0.0, hi=2^-10, order=2)" in {
        val tes2 = new Remez(math.exp(_), math.exp(_), 0.0, math.pow(2,-10), 2)
        tes2.coeffs.toArray should be (exp2.toArray)
    }

}

