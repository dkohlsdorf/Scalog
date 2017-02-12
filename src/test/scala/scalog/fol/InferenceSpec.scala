package scalog.fol

import org.scalatest.FunSuite

class InferenceSpec extends FunSuite {

  final val EvilKing = Clause(
    Predicate("evil", List(Variable("X"))), List(
      Predicate("king",   List(Variable("X"))),
      Predicate("greedy", List(Variable("X")))
    )
  )

  final val FamilyKB = KnowledgeBase()

  // small family tree
  FamilyKB tell Predicate("parent", List(Atom("daniel"),   Atom("wolfgang")))
  FamilyKB tell Predicate("parent", List(Atom("wolfgang"), Atom("werner")))
  FamilyKB tell Predicate("parent", List(Atom("werner"),   Atom("dontknow")))
  FamilyKB tell Predicate("gender", List(Atom("dontknow"), Atom("female")))

  // ancestor recursion
  FamilyKB tell (
    Predicate("ancestor", List(Variable("X"), Variable("Y"))), List(
    Predicate("parent",   List(Variable("X"), Variable("Z"))),
    Predicate("ancestor", List(Variable("Z"), Variable("Y")))
  ))

  // ancestor stop condition
  FamilyKB tell (
    Predicate("ancestor", List(Variable("X"), Variable("Y"))), List(
    Predicate("parent", List(Variable("X"), Variable("Y")))
  ))

  test("The inference should return no substitutions for false queries") {
    assert(!Result.isOk(
      FamilyKB ask List(
        Predicate("ancestor", List(Atom("notinset"), Variable("X")))
      )
    ))
  }

  test("The inference should return an empty substitutions for true queries") {
    assert(Result.isOk(
      FamilyKB ask List(
        Predicate("ancestor", List(Atom("daniel"), Variable("werner")))
      )
    ))
  }

  test("The inference should handle recursive functions") {
    val answer = FamilyKB ask List(
      Predicate("ancestor", List(Atom("daniel"), Variable("P"))),
      Predicate("gender",   List(Variable("P"),  Atom("female")))
    )
    assert(answer(0).get(Variable("P")).get == Atom("dontknow"))
  }

  test("Return all solutions") {
    val answer = FamilyKB ask List(
      Predicate("ancestor", List(Atom("daniel"), Variable("P")))
    )
    assert(answer.map(x => x(Variable("P"))).toSet == Set(Atom("dontknow"), Atom("wolfgang"), Atom("werner")))
  }

  test("The inference should substitute") {
    val substitution = Map(Variable("X") -> Atom("y"))
    assert(Inference.substitute(EvilKing.body, substitution) ==  List(
      Predicate("king",   List(Atom("y"))),
      Predicate("greedy", List(Atom("y")))
    ))
  }

  test("The inference should assign atoms to variables") {
    val substitution = Map(
      Variable("X")  -> Variable("X_1"),
      Variable("X_1") -> Atom("x")
    )
    assert(Inference.assign(EvilKing.head :: EvilKing.body, substitution) == Map( Variable("X") -> Atom("x") ))
  }

}
