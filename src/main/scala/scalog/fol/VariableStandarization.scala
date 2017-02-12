package scalog.fol

/**
  * Variable standarization renames variables
  * before unification. So that both terms X and Y
  * do not share any variables.
  *
  * "The simplest way of generating new variable names is to
  * keep a numeric global "index" variable. Every time you take
  * a new rule from the KB, you would append that index to its
  * variable names, and increment it for next time." [1]
  *
  * [1] https://www.cs.unc.edu/~lazebnik/fall10/assignment3.html
  *
  * by Daniel Kohlsdorf
  */
class VariableStandarization(var idx: Int = 0) {

  def standarizeAppart(clause: Clause): Clause = {
    next
    clause match {
      case Clause(Predicate(functor, body), terms) => Clause(
        Predicate(functor, standarize(body)),
        standarize(terms).map(_.asInstanceOf[Predicate])
      )
    }
  }

  def standarize(terms: List[Term]): List[Term] =
    terms.map {
      case term: Atom[_] => term
      case Predicate(functor, body) => Predicate(functor, standarize(body))
      case x: Variable => Variable(x.name + s"_${idx}")
    }

  def next: Unit = idx += 1

}
