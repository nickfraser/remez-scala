package Remez

import breeze.linalg._
import breeze.numerics._
import scala.util.control.Breaks._

/** An iterative algorithm to compute the least squares polynomial approximation to a function.
  *
  *
  */
class Remez(f: Double => Double, fd: Double => Double, low: Double, hi: Double, order: Int, iter: Int=10, thresh: Double=math.pow(2,-30)) {
    /** Find the root of err between y1 and y2. */
    def findzero(err: Double => Double, y1: Double, y2: Double): Double = {
        // Get the value of the function at each point.
        var x1 = y1
        var x2 = y2
        var fx1 = err(x1)
        var fx2 = err(x2)

        if (signum(fx1) == signum(fx2)) { // Check that the signs are different.
            throw new Exception(f"The signs of f(x1) and f(x2) are the same. x1 = $x1%f, x2 = $x2%f, f(x1) = $fx1%f, f(x2) = $fx2%f")
        }

        // Find a closer point to the root using a linear fit between
        var x = x1 - fx1*((x2-x1)/(fx2-fx1))
        var fx = err(x)

        while (math.abs(fx) > math.pow(2,-52)) {
            // Shift the interval.
            if (signum(fx) == signum(fx1)) { 
                x1 = x
                fx1 = fx
            } else {
                x2 = x
                fx2 = fx
            }
            x = x1 - fx1*((x2-x1)/(fx2-fx1))
            fx = err(x)
        }
        x
    }

    var y = linspace(low, hi, order+2) // Estimate starting point for coefficients.
    var coeffs = DenseVector.zeros[Double](order+1)
    var maxErr = 0.0
    val e = DenseVector.tabulate(order+2){ i => math.pow(-1,i+1) } // Equivalent to (-1).^[1:order+2] in MATLAB.
    val t = DenseVector.range(1,order+1) // Array storing the powers of the polynomials starting from 1->order.
    val powers = DenseVector.range(0,order+1) // Array storing the powers of the polynomials starting from 0->order+1.

    breakable { for(i <- 0 until iter) {
        val h = y-low // Center the points around 'low'.
        val hM = DenseMatrix.tabulate(h.length,powers.length){ (i, j) => math.pow(h(i),powers(j))} // Create the set of equations.
        val M = DenseMatrix.horzcat(hM,e.toDenseMatrix.t) // Concatenate e to polynomial matrices.
        val n = DenseVector.tabulate(M.rows){ i => f(y(i)) } // Evaluate f at y.

        // Solve system of equations.
        val a = M\n
        // Extract the coefficients for the current polynomial.
        val a1 = a(0 to M.cols-2)
        val a1d = a(1 to M.cols-2):*convert(t, Double) // Extract the coefficients for the first derivative.

        // Find the zeros of the error function.
        val err: Double => Double = (x => f(x) - polyval(a1.toArray, x-low))
        val z = DenseVector.zeros[Double](order+3)
        z(0 to 0) := low
        z(order+2 to order+2) := hi
        z(1 to order+1) := DenseVector.tabulate(order+1){ i => findzero(err, y(i), y(i+1)) }

        // Between each point in z, attempt to find the point which maximises the error function.
        val y1 = DenseVector.zeros[Double](order+2)
        val v = DenseVector.zeros[Double](order+2)
        val errDiv: Double => Double = (x => fd(x) - polyval(a1d.toArray, x-low))
        for(j <- 0 until order+2) {
            if (signum(errDiv(z(j))) != signum(errDiv(z(j+1)))) {
                // Error is between z(j) and z(j+1), find the root.
                y1(j to j) := findzero(errDiv, z(j), z(j+1))
                v(j to j) := math.abs(err(y1(j)))
            } else {
                // Error is not between z(j) and z(j+1), select the one with the biggest error.
                val v1 = math.abs(err(z(j)))
                val v2 = math.abs(err(z(j+1)))
                if (v1 > v2) {
                    y1(j to j) := z(j)
                    v(j to j) := v1
                } else {
                    y1(j to j) := z(j+1)
                    v(j to j) := v2
                }
            }
        }
        // Check the stopping criteria.
        coeffs := a1
        maxErr = a(M.cols-1)
        val ind = argmax(v)
        if (math.abs(y(ind) - y1(ind)) < thresh) {
            break
        } else if((ind<y.length-1) && math.abs(y(ind+1)-y1(ind)) < thresh) {
            break
        }
        y = y1
    }}
}

