package scalog.fol

import org.scalatest.FunSuite

class GraphSearchSpec extends FunSuite {

  import ListBuilder._

  final val graphKB = KnowledgeBase()

  graphKB tell Predicate("connected", List(Atom(1), Atom(2)))
  graphKB tell Predicate("connected", List(Atom(2), Atom(3)))
  graphKB tell Predicate("connected", List(Atom(3), Atom(4)))
  graphKB tell Predicate("connected", List(Atom(4), Atom(5)))
  graphKB tell Predicate("connected", List(Atom(5), Atom(6)))
  graphKB tell Predicate("connected", List(Atom(9), Atom(10)))

  graphKB tell (
    Predicate("path", List(Variable("FROM"), Variable("TO"), Predicate(ListFunctor, List(Variable("FROM"), End)))), List(
      Predicate("connected", List(Variable("FROM"), Variable("TO")))
  ))

  graphKB tell (
    Predicate("path", List(Variable("FROM"), Variable("TO"), Predicate(ListFunctor, List(Variable("FROM"), Variable("PATH"))))), List(
      Predicate("connected", List(Variable("FROM"), Variable("X"))),
      Predicate("path",      List(
        Variable("X"),
        Variable("TO"),
        Variable("PATH"))
      ))
  )

  test("should find a path") {
    val answers = graphKB ask List(
      Predicate("path", List(Atom(1), Atom(6), Variable("X")))
    )
    assert(Result.isOk(answers))
    assert(answers.size == 1)
    val answer = answers(0)
    assert(Result.hasSubstitutions(answer))
    assert(flatten(answer(Variable("X")).asInstanceOf[Predicate]) == List(Atom(1), Atom(2), Atom(3), Atom(4), Atom(5)))

    val noAnswer = graphKB ask List(
      Predicate("path", List(Atom(1), Atom(10), Variable("X")))
    )
    assert(!Result.isOk(noAnswer))
  }

}
