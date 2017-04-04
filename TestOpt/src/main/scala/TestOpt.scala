import org.coinor.Ipopt

object TestOpt {
   def main(args: Array[String]): Unit = {
     HS071()
     val status = HS071.OptimizeNLP()
     if( status == Ipopt.SOLVE_SUCCEEDED )
       println("\n\n*** The problem solved!")
     else
       println("\n\n*** The problem was not solved successfully!")
     val obj = HS071.getObjectiveValue()
     println("\nObjective Value = " + obj + "\n")

     val x = HS071.getVariableValues()
     HS071.print(x, "Primal Variable Values:")

     val constraints = HS071.getConstraintValues()
     HS071.print(constraints, "Constraint Values:")

     val MLB = HS071.getLowerBoundMultipliers()
     HS071.print(MLB, "Dual Multipliers for Variable Lower Bounds:")

     val MUB = HS071.getUpperBoundMultipliers()
     HS071.print(MUB, "Dual Multipliers for Variable Upper Bounds:")

     val lam = HS071.getConstraintMultipliers()
     HS071.print(lam, "Dual Multipliers for Constraints:")
   }


}
