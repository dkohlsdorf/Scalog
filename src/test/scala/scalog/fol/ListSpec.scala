package scalog.fol

import org.scalatest.FunSuite

class ListSpec extends FunSuite {

  import ListBuilder._

  final val PListsKB = KnowledgeBase()

  // Prolog List(1,2,3) saved as binary tree
  val PrologList =  Predicate(ListFunctor, List(
    Atom(1), Predicate(ListFunctor, List(
      Atom(2), Predicate(ListFunctor, List(
        Atom(3), End
      ))
    ))
  ))

  PListsKB tell Predicate("member", List(Variable("X"), Predicate(ListFunctor, List(Variable("X"), Variable("T")))))
  PListsKB tell (
    Predicate("member", List(Variable("X"), Predicate(ListFunctor, List(Variable("H"), Variable("T"))))), List(
      Predicate("member", List(Variable("X"), Variable("T")))
  ))

  test("ListBuilder should build prologlist") {
    assert(ListBuilder(List(Atom(1), Atom(2), Atom(3))) == PrologList)
  }
  
  test("The inference should find members in lists") {
    assert(
      Result.isOk(PListsKB ask List(Predicate("member", List(Atom(1), PrologList)))) &&
      Result.isOk(PListsKB ask List(Predicate("member", List(Atom(2), PrologList)))) &&
      Result.isOk(PListsKB ask List(Predicate("member", List(Atom(3), PrologList)))) &&
      !Result.isOk(PListsKB ask List(Predicate("member", List(Atom(4), PrologList))))
    )
  }

  test("Listbuilder should flatten a prologlist") {
    assert(ListBuilder.flatten(PrologList) == List(Atom(1), Atom(2), Atom(3)))
  }

}

