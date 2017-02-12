package scalog.fol

/**
  * The basic data model of a prolog style system.
  *
  * Constants and Atoms are Terms.
  * Predicates are Terms and represent a composition of Terms.
  * Clauses represent the logic: head <- [X1 & X2 & X3 ... XN].
  * All clause components are predicates. [X1 .. XN] is called
  * the clause body. Facts are represented by clauses with an
  * empty body and the head predicate is variable free.
  * Rules are represented by clauses with a head containing
  * variables and a body with at least one element.
  *
  * Prolog lists are just binary trees with the left child being the
  * head of the list and the right side the tail of the list.
  * This can be implemented as a regular prolog predicate:
  * list(h1, list(h2, list(h3, nil))).
  *
  * [1] Ivan Bratko: "Prolog Programming for Artificial Intelligence",
  *     4th ed., Chapter 2, Adison Wesley, 2011
  *
  * by Daniel Kohlsdorf
  */
sealed trait Term

case class Atom[T](val x: T) extends Term

case class Variable(name: String) extends Term

case class Predicate(functor: String, terms: List[Term]) extends Term {

  def arity: Int = terms.size

  def matches(other: Predicate): Boolean = other.functor == functor && other.arity == arity

}

case class Clause(head: Predicate, body: List[Predicate])

object ListBuilder {

  final val ListFunctor = "."

  final val End = Atom("nil")

  def split(head: String, tail: String) = Predicate(ListFunctor, List(Variable(head), Variable(tail)))

  def split(head: Term, tail: Term) = Predicate(ListFunctor, List(head, tail))

  def apply(x: List[Term]):Term = binary(x)

  def binary(x: List[Term]): Term =
    if(x.isEmpty) End
    else Predicate(ListFunctor, List(x.head, binary(x.tail)))

  def flatten(x: Predicate): List[Term] = x match {
    case Predicate(_, List(h, End)) => List(h)
    case Predicate(_, List(h, tail)) => h :: flatten(tail.asInstanceOf[Predicate])
   }

}