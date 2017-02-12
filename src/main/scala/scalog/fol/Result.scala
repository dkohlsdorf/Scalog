package scalog.fol

import scalog.fol.Inference._

/**
  * Handle Results from Inference
  *
  * by Daniel Kohlsdorf
  */
object Result {

  def isOk(solutions: Solutions): Boolean = solutions.nonEmpty

  def hasSubstitutions(substitution: Substitution): Boolean = substitution.nonEmpty

  def printSubstitution(substitution: Substitution): Unit = for {
    (v, t) <- substitution
  } if (t.isInstanceOf[Predicate]) {
    val predicate = t.asInstanceOf[Predicate]
    if(predicate.functor == ListBuilder.ListFunctor)
      println(s"   ${v} = ${ListBuilder.flatten(predicate).map{
        case Atom(x) => x
        case x       => x
      }.mkString(" ")}")
    else println(s"   ${v} = ${t}")
  } else println(s"   ${v} = ${t}")

  def printSubstitution(solutions: Solutions): Unit = for {
    solution <- solutions
  } {
    printSubstitution(solution)
    println(";")
  }

  def printResult(answer: Solutions): Unit =   if(isOk(answer)) {
    println("true")
    if(hasSubstitutions(answer(0))) printSubstitution(answer)
  } else println("false")

}
