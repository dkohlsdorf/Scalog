package scalog.fol

import org.scalatest.FunSuite

class VariableStandarizationSpec extends FunSuite {

  final val EvilClause = Clause(Predicate("evil", List(Variable("X"))),
    List(
      Predicate("king",   List(Variable("X"))),
      Predicate("greedy", List(Variable("X")))
    )
  )

  final val Expected1 = Clause(Predicate("evil", List(Variable("X_1"))),
    List(
      Predicate("king",   List(Variable("X_1"))),
      Predicate("greedy", List(Variable("X_1")))
    )
  )

  final val Expected2 = Clause(Predicate("evil", List(Variable("X_2"))),
    List(  
      Predicate("king",   List(Variable("X_2"))),
      Predicate("greedy", List(Variable("X_2")))
    )
  )

  test("standarization should replace all variables in a clause with an unused name") {
    val std = new VariableStandarization
    assert(std.standarizeAppart(EvilClause) == Expected1)
    assert(std.standarizeAppart(EvilClause) == Expected2)
  }

}
