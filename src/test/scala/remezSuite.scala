
import Remez._
import breeze.linalg._
import org.scalatest._

/** A test suite for the Remez class.
  *
  * Currently tests sin(x) and exp(x).
  */
class RemezSuite extends FlatSpec with Matchers {
    val tol = 1.0e-9

    // Test 1.
    val exp1 = DenseVector(-4.850637758584954e-12, 1.000000089406958e+00, -2.441406092672303e-04)
    val tol1 = exp1.max*tol
    "The Remez class" should f"find the coeffs for test 1 (params: f(x)=sin(x), low=0.0, hi=2^-10, order=2) with a tolerance of $tol1%e" in {
        val tes1 = new Remez(math.sin(_), math.cos(_), 0.0, math.pow(2,-10), 2)
        (tes1.coeffs.toArray, exp1.toArray).zipped.map((x,y) => x should be (y +- tol1))
    }

    // Test 2.
    val exp2 = DenseVector(1.000000000004853e+00, 9.999999105564560e-01, 5.002442127611959e-01)
    val tol2 = exp2.max*tol
    "The Remez class" should f"find the coeffs for test 2 (params: f(x)=exp(x), low=0.0, hi=2^-10, order=2) with a tolerance of $tol2%e" in {
        val tes2 = new Remez(math.exp(_), math.exp(_), 0.0, math.pow(2,-10), 2)
        (tes2.coeffs.toArray, exp2.toArray).zipped.map((x,y) => x should be (y +- tol2))
    }

    // Test 2.
    val exp3 = DenseVector(-5.748063834086523e-03, 4.740601769798286e-02, -5.826028714481889e-02, 2.567153397694599e-02, -4.658388047107521e-03, 3.139040166568819e-04)
    val tol3 = exp3.max*tol
    "The Remez class" should f"find the coeffs for test 3 (params: f(x)=exp(x), low=-8.0, hi=0.0, order=5) with a tolerance of $tol3%e" in {
        val tes3 = new Remez(math.exp(_), math.exp(_), -8.0, 0.0, 5)
        (tes3.coeffs.toArray, exp3.toArray).zipped.map((x,y) => x should be (y +- tol3))
    }

}

