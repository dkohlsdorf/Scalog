package scalog.fol

import scala.collection.mutable
import org.scalatest.FunSuite

class UnificationSpec extends FunSuite {

  test("The variables should only unify if assignment does not conflict") {
    val query  = Predicate("x", List(Variable("X"), Variable("X")))
    val atoms1 = Predicate("x", List(Atom(1), Atom(1)))
    val atoms2 = Predicate("x", List(Atom(1), Atom(2)))

    val sub = mutable.Map.empty[Variable, Term]
    val unified1 = Unification.unify(query, atoms1, sub)
    val unified2 = Unification.unify(query, atoms2, sub)
    assert(unified1 && !unified2)
  }

  test("The same atom should unify") {
    val sub = mutable.Map.empty[Variable, Term]
    val unified = Unification.unify(Atom("x"), Atom("x"), sub)
    assert(sub.isEmpty && unified)
  }

  test("Two different atoms should not unify") {
    val sub = mutable.Map.empty[Variable, Term]
    val unified = Unification.unify(Atom("x"), Atom("y"), sub)
    assert(sub.isEmpty && !unified)
  }

  test("A variable and an atom should unify") {
    val sub = mutable.Map.empty[Variable, Term]
    val unified = Unification.unify(Variable("x"), Atom("y"), sub)
    assert(sub.contains(Variable("x")) && unified)
  }

  test("An atom and a variable should unify") {
    val sub = mutable.Map.empty[Variable, Term]
    val unified = Unification.unify(Atom("x"), Variable("y"), sub)
    assert(sub.contains(Variable("y")) && unified)
  }

  test("Two predicates with the same functor unify recursively") {
    val sub = mutable.Map.empty[Variable, Term]
    val unified = Unification.unify(
      Predicate("xy", List(Atom("x"), Variable("y"))),
      Predicate("xy", List(Atom("x"), Atom("b"))),
      sub
    )
    assert(sub(Variable("y")) == Atom("b") && unified)
  }

  test("Two predicates with different functor names do not unify") {
    val sub = mutable.Map.empty[Variable, Term]
    val unified = Unification.unify(
      Predicate("xy", List(Atom("x"), Variable("y"))),
      Predicate("xz", List(Atom("x"), Atom("b"))),
      sub
    )
    assert(sub.isEmpty && !unified)
  }

  test("Two predicates with different arity do not unify") {
    val sub = mutable.Map.empty[Variable, Term]
    val unified = Unification.unify(
      Predicate("xy", List(Atom("x"), Variable("y"))),
      Predicate("xy", List()),
      sub
    )
    assert(sub.isEmpty && !unified)
  }

}
