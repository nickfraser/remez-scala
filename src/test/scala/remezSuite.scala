
import Remez._
import breeze.linalg._
import breeze.numerics._
import org.scalatest._

/** A test suite for the Remez class.
  *
  * Currently tests sin(x) and exp(x).
  */
class RemezSuite extends FlatSpec with Matchers {
    val coeffTol = 1.0e-9
    val errTol = 1.0e-5

    // Test 1.
    it should f"find the coeffs for test 1 (params: f(x)=exp(x), low=0.0, hi=2^-10, order=2)" in {
        val exp = DenseVector(1.000000000004853e+00, 9.999999105564560e-01, 5.002442127611959e-01)
        val expErr = 4.853011733229117e-12
        val coeffTolV = max(abs(exp))*coeffTol
        val errTolV = abs(expErr*errTol)
        val tes = new Remez(math.exp(_), math.exp(_), 0.0, math.pow(2,-10), 2)
        (tes.coeffs.toArray, exp.toArray).zipped.map((x,y) => x should be (y +- coeffTolV))
        tes.maxErr should be (expErr +- errTolV)
    }

    // Test 2.
    it should f"find the coeffs for test 2 (params: f(x)=sin(x), low=0.0, hi=2^-10, order=2)" in {
        val exp = DenseVector(-4.850637758584954e-12, 1.000000089406958e+00, -2.441406092672303e-04)
        val expErr = -4.850637758584954e-12
        val coeffTolV = max(abs(exp))*coeffTol
        val errTolV = abs(expErr*errTol)
        val tes = new Remez(math.sin(_), math.cos(_), 0.0, math.pow(2,-10), 2)
        (tes.coeffs.toArray, exp.toArray).zipped.map((x,y) => x should be (y +- coeffTolV))
        tes.maxErr should be (expErr +- errTolV)
    }

    // Test 3.
    it should f"find the coeffs for test 3 (params: f(x)=exp(x), low=-8.0, hi=0.0, order=5)" in {
        val exp = DenseVector(-5.748063834086523e-03, 4.740601769798286e-02, -5.826028714481889e-02, 2.567153397694599e-02, -4.658388047107521e-03, 3.139040166568819e-04)
        val expErr = -6.083526461989035e-03
        val coeffTolV = max(abs(exp))*coeffTol
        val errTolV = abs(expErr*errTol)
        val tes = new Remez(math.exp(_), math.exp(_), -8.0, 0.0, 5)
        (tes.coeffs.toArray, exp.toArray).zipped.map((x,y) => x should be (y +- coeffTolV))
        tes.maxErr should be (expErr +- errTolV)
    }

    // Test 4.
    it should f"find the coeffs for test 4 (params: f(x)=sin(x), low=-8.0, hi=0.0, order=5)" in {
        val exp = DenseVector(-9.099241668557152e-01, -8.794578655807470e-01, 1.576746585780616e+00, -5.256088365335613e-01, 6.263299793032073e-02, -2.451165635256970e-03)
        val expErr = 7.943407976766663e-02
        val coeffTolV = max(abs(exp))*coeffTol
        val errTolV = abs(expErr*errTol)
        val tes = new Remez(math.sin(_), math.cos(_), -8.0, 0.0, 5)
        (tes.coeffs.toArray, exp.toArray).zipped.map((x,y) => x should be (y +- coeffTolV))
        tes.maxErr should be (expErr +- errTolV)
    }

}

