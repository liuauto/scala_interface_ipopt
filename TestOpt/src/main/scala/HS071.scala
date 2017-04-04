import org.coinor.Ipopt

object HS071 extends Ipopt {
  var n: Int = 0
  var m: Int = 0
  var nele_jac: Int = 0
  var nele_hess: Int = 0
  def apply() {
    nele_jac = 8
    nele_hess = 10
    n = 4
    m = 2
    val	index_style = Ipopt.C_STYLE
//    val count_bounds = 0
//    val dcount_start = 0
    create(n, m, nele_jac, nele_hess, index_style)
  }

  def eval_h(n: Int,
             x: Array[Double],
             new_x: Boolean,
             obj_factor: Double,
             m: Int,
             lambda: Array[Double],
             new_lambda: Boolean,
             nele_hess: Int,
             iRow: Array[Int],
             jCol: Array[Int],
             values: Array[Double]): Boolean = {
    require(n == this.n)
    require(m == this.m)
    var idx = 0 /* nonzero element counter */
    var row = 0 /* row counter for loop */
    var col = 0 /* col counter for loop */
    if( values == null )
    {
      /* return the structure. This is a symmetric matrix, fill the lower left triangle only. */

      /* the hessian for this problem is actually dense */
      idx = 0
      for( row <- 0 until n)
      {
        //for( col = 0; col <= row; ++col)
        for( col <- 0 until row )
        {
          iRow(idx) = row
          jCol(idx) = col
          idx += 1
        }
      }
  //    require(idx == this.nele_hess)
//      require(nele_hess == this.nele_hess)
    }
    else
    {
      /* return the values. This is a symmetric matrix, fill the lower left triangle only */

      /* fill the objective portion */
      values(0) = obj_factor * (2*x(3))                /* 0,0 */
      values(1) = obj_factor * x(3)                  /* 1,0 */
      values(2) = 0.0                             /* 1,1 */
      values(3) = obj_factor * x(3)                 /* 2,0 */
      values(4) = 0.0                                 /* 2,1 */
      values(5) = 0.0                                 /* 2,2 */
      values(6) = obj_factor * (2*x(0) + x(1) + x(2)) /* 3,0 */
      values(7) = obj_factor * x(0)                 /* 3,1 */
      values(8) = obj_factor * x(0)                 /* 3,2 */
      values(9) = 0.0                                 /* 3,3 */

      /* add the portion for the first constraint */
      values(1) += lambda(0) * (x(2) * x(3))          /* 1,0 */
      values(3) += lambda(0) * (x(1) * x(3))          /* 2,0 */
      values(4) += lambda(0) * (x(0) * x(3))          /* 2,1 */
      values(6) += lambda(0) * (x(1) * x(2))          /* 3,0 */
      values(7) += lambda(0) * (x(0) * x(2))          /* 3,1 */
      values(8) += lambda(0) * (x(0) * x(1))          /* 3,2 */

      /* add the portion for the second constraint */
      values(0) += lambda(1) * 2.0                    /* 0,0 */
      values(2) += lambda(1) * 2.0                    /* 1,1 */
      values(5) += lambda(1) * 2.0                    /* 2,2 */
      values(9) += lambda(1) * 2.0                    /* 3,3 */
    }
    true
  }

  def eval_jac_g(n: Int,
                 x: Array[Double],
                 new_x: Boolean,
                 m: Int,
                 nele_jac: Int,
                 iRow: Array[Int],
                 jCol: Array[Int],
                 values: Array[Double]): Boolean = {
    require(n == this.n)
    require(m == this.m)
    if( values == null )
    {
      /* return the structure of the jacobian */

      /* this particular jacobian is dense */
      iRow(0) = 0;  jCol(0) = 0
      iRow(1) = 0;  jCol(1) = 1
      iRow(2) = 0;  jCol(2) = 2
      iRow(3) = 0;  jCol(3) = 3
      iRow(4) = 1;  jCol(4) = 0
      iRow(5) = 1;  jCol(5) = 1
      iRow(6) = 1;  jCol(6) = 2
      iRow(7) = 1;  jCol(7) = 3
    }
    else
    {
      /* return the values of the jacobian of the constraints */

      values(0) = x(1)*x(2)*x(3) /* 0,0 */
      values(1) = x(0)*x(2)*x(3) /* 0,1 */
      values(2) = x(0)*x(1)*x(3) /* 0,2 */
      values(3) = x(0)*x(1)*x(2) /* 0,3 */

      values(4) = 2*x(0)         /* 1,0 */
      values(5) = 2*x(1)         /* 1,1 */
      values(6) = 2*x(2)         /* 1,2 */
      values(7) = 2*x(3)         /* 1,3 */
    }

    true
  }

  def eval_g(n: Int,
             x: Array[Double],
             new_x: Boolean,
             m: Int,
             g: Array[Double]): Boolean = {
    require(n == this.n)
    require(m == this.m)
    g(0) = x(0) * x(1) * x(2) * x(3)
    g(1) = x(0)*x(0) + x(1)*x(1) + x(2)*x(2) + x(3)*x(3)
    true
  }

  def eval_grad_f(n: Int,
                  x: Array[Double],
                  new_x: Boolean,
                  grad_f: Array[Double]): Boolean = {
    require( n == this.n)
    grad_f(0) = x(0) * x(3) + x(3) * (x(0) + x(1) + x(2))
    grad_f(1) = x(0) * x(3)
    grad_f(2) = x(0) * x(3) + 1
    grad_f(3) = x(0) * (x(0) + x(1) + x(2))
    true
  }

  def eval_f(n: Int,
             x: Array[Double],
             new_x: Boolean,
             obj_value: Array[Double]): Boolean = {
    require(n == this.n)
    obj_value(0) = x(0)*x(3)*(x(0) + x(1) + x(2)) + x(2)
    true
  }

  def get_bounds_info(n: Int,
                      x_L: Array[Double],
                      x_U: Array[Double],
                      m: Int,
                      g_L: Array[Double],
                      g_U: Array[Double]): Boolean = {
    require(n == this.n)
    require(m == this.m)
    for( i <- 0 until n) {
      x_L(i) = 1.0
      x_U(i) = 5.0
    }
    g_L(0) = 25.0
    g_U(0) = 2e19
    g_L(1) = 40.0
    g_U(1) = 40.0
    true
  }

  def get_starting_point(n: Int,
                         init_x: Boolean,
                         x: Array[Double],
                         init_z: Boolean,
                         z_L: Array[Double],
                         z_U: Array[Double],
                         m: Int,
                         init_lambda: Boolean,
                         lambda: Array[Double]): Boolean = {
    require(!init_z)
    require(!init_lambda)
    if(init_x){
      x(0) = 1.0
      x(1) = 5.0
      x(2) = 5.0
      x(3) = 1.0
    }
    true
  }

  override def get_scaling_parameters(obj_scaling: Array[Double],
                                      n: Int, x_scaling: Array[Double],
                                      m: Int, g_scaling: Array[Double],
                                      use_x_g_scaling: Array[Boolean]): Boolean = {
    false
  }


  def print(x: Array[Double], str: String): Unit = {
    println(str)
    for( i <- x.indices)
      println(x(i))
    println()
  }


}
