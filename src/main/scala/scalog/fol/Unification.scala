package scalog.fol

import scala.collection.mutable

/**
  * Unification of two terms X and Y.
  * Unification returns true if X and Y can be unified.
  * Furthermore, unification assigns a substitution
  * from each variable x in X to a term in Y.
  * Also, unification assigns a substitution for
  * each variable y in Y to a term in X.
  *
  * If X is an atom and Y is an atom, unification succeeds if X and Y are equal.
  * If X or Y is a variable, unification succeeds and the variable is bound to the other term:
  *   before assignment check if a substitution with the same name exists.
  *   if yes unify the substitution
  * If X is a predicated(x_name, x1, .. ,xn) and Y is a predicate(y_name, y1, ..., ym) unification
  * succeeds if x_name == y_name and n == m and all pairs (xi, yi) unify. Otherwise, unifcation fails.
  *
  * [1] Ivan Bratko: "Prolog Programming for Artificial Intelligence",
  *     4th ed., Chapter 2, Adison Wesley, 2011
  * [2] Stuart Russel, Peter Norvig: "Artificial Intelligence a Modern Approach", Pearson, 2011
  *
  * by Daniel Kohlsdorf
  */
object Unification {

  def unify(x: Term, y: Term, subs: mutable.Map[Variable, Term]): Boolean = (x, y) match {
    case (Atom(x), Atom(y)) => x == y
    case (x: Variable, y) => unifyVar(x, y, subs)
    case (x, y: Variable) => unifyVar(y, x, subs)
    case (x: Predicate, y: Predicate) if x matches y => {
      x.terms.zip(y.terms).map{ case (t1, t2) => unify(t1, t2, subs)}.reduce(_ && _)
    }
    case _ => false
  }

  def unifyVar(x: Variable, y: Term, subs: mutable.Map[Variable, Term]): Boolean = {
    if(subs.contains(x)) unify(subs(x), y, subs)
    else if(y.isInstanceOf[Variable] && subs.contains(y.asInstanceOf[Variable])) unify(x, subs(y.asInstanceOf[Variable]), subs)
    else {
      subs += (x -> y)
      true
    }
  }

}


unification(
  Predicate(parent, List(Atom(sam), Atom(peter))),
  Predicate(parent, List(Atom(sam), Atom(peter)))
)
