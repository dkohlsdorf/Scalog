package scalog.fol

import org.scalatest.FunSuite

class PrologParserSpec extends FunSuite {

  final val AncestorProgram = PrologParser.knowledge(
    """
      |
      |ancestor(A, B):-
      | parent(A, B).
      |
      |ancestor(A, B):-
      |  parent(A, Z),
      |  ancestor(Z, B).
      |
      |parent(daniel, wolfgang).
      |parent(wolfgang, werner).
      |
      |
      |""".stripMargin
  )

  final val ListProgram = PrologParser.knowledge(
    """
      |member(X, [X|T]).
      |member(X, [H|T]):-
      | member(X, T).
    """.stripMargin) // cut doesnt work

  final val ListQuery = PrologParser.query("member(daniel, [werner, daniel, wolfgang])")

  final val AncestorQuery = PrologParser.query("ancestor(daniel, X)")

  final val AncestorResult = List(Map(Variable("X") -> Atom("wolfgang")), Map(Variable("X") -> Atom("werner")))

  test("Parse the ancestor program") {
    assert(AncestorQuery   != None)
    assert(AncestorProgram != None)
    for {
      q <- AncestorQuery
      p <- AncestorProgram
    } assert( (p ask q) == AncestorResult)
  }

  test("Parse the membership program") {
    assert(ListQuery != None)
    assert(ListProgram != None)
    for {
      q <- ListQuery
      p <- ListProgram
    } assert(Result.isOk(p ask q))
  }

}
