package scalog.fol

import scala.collection.mutable

/**
  * Inference in a prolog program.
  * Given a set of clauses (a program)
  * the inference tries to find a substitution
  * such that the query evaluates true.
  *
  * After search, the program finds a path through the
  * renamings in the substitution list such that all variables in the
  * goal clause map to a final atom.
  *
  * [1] Ivan Bratko: "Prolog Programming for Artificial Intelligence",
  *     4th ed., Chapter 2, Adison Wesley, 2011
  * 
  * by Daniel Kohlsdorf
  */
class Inference(standarizer: VariableStandarization, program: Map[String, List[Clause]]) {

  import Unification._
  import Inference._

  def inferAll(goals: List[Predicate]): Solutions = {
    val x = inferAll(goals, Map.empty[Variable, Term])
    x.map(solution => assign(goals, solution))
  }

  protected def inferAll(goals: List[Predicate], solution: Substitution): Solutions =
    if(goals.isEmpty) List(solution)
    else {
      val goal   = goals.head
      val other  = goals.tail
      val answer = for (clause <- program(goal.functor)) yield {
        val Clause(standarizedHead, standarizedBody) = standarizer.standarizeAppart(clause)
        val substitution = mutable.Map.empty[Variable, Term] ++ solution
        val unifyable    = unify(goal, standarizedHead, substitution)
        if(unifyable) {
          val newGoals = substitute(standarizedBody ::: other, substitution.toMap)
          val x = inferAll(newGoals.collect{case x: Predicate => x}, substitution.toMap)
          x
        } else Nil
      }
      answer.flatten
    }

}

object Inference {

  type Substitution = Map[Variable, Term]

  type Solutions    = List[Substitution]

  def substitute(x: List[Term], subs: Substitution): List[Term] = x.map {
    case term: Variable           => subs.get(term).getOrElse(term)
    case Predicate(functor, body) => Predicate(functor, substitute(body, subs))
    case term                     => term
  }

  def assign(predicate: List[Predicate], substitution: Substitution): Substitution = {
    val variables: List[Variable] = predicate.flatMap {
      case Predicate(_, body) => body.flatMap(getAllVariables)
    }.distinct
    variables.map(
      x => x -> findAssignment(x, substitution)
    ).toMap
  }

  def getAllVariables(term: Term): List[Variable] = term match {
    case x: Atom[_]         => List.empty
    case x: Variable        => List(x)
    case Predicate(_, body) => body.flatMap(getAllVariables)
  }

  def findAssignment(x: Term, substitution: Substitution): Term = x match {
    case x: Variable => findAssignment(substitution(x), substitution)
    case Predicate(functor, body) => Predicate(functor, body.map(x => findAssignment(x, substitution)))
    case x => x
  }

  def apply(program: Map[String, List[Clause]]): Inference = new Inference(new VariableStandarization, program)
  
}
