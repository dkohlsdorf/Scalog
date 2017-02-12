package scalog.fol

import org.scalatest.FunSuite

class QuicksortSpec extends FunSuite {

  import ListBuilder._

  final val sortKB = KnowledgeBase()

  for {
    i <- 0 until 10
    j <- 0 until 10
    if j <= i
  } sortKB tell Predicate("<=", List(Atom(j), Atom(i)))

  for {
    i <- 0 until 10
    j <- 0 until 10
    if j > i
  } sortKB tell Predicate(">", List(Atom(j), Atom(i)))

  sortKB tell (
    Predicate("sort", List(split("H", "T"), Variable("Sorted"))), List(
       Predicate("pivot",  List(Variable("H"),  Variable("T"), Variable("Left"), Variable("Right"))),
       Predicate("sort",   List(Variable("Left"),  Variable("Ls"))),
       Predicate("sort",   List(Variable("Right"), Variable("Rs"))),
       Predicate("append", List(Variable("Ls"), split("H", "Rs"), Variable("Sorted")))
  ))
  sortKB tell Predicate("sort", List(End, End))

  sortKB tell Predicate("append", List(End, Variable("As"), Variable("As")))
  sortKB tell (
    Predicate("append", List(split("A", "As"), Variable("Bs"), split("A", "Cs"))), List(
    Predicate("append", List(Variable("As"), Variable("Bs"), Variable("Cs")))
  ))

  sortKB tell Predicate("pivot", List(Variable("H"), End, End, End))
  sortKB tell (
    Predicate("pivot", List(Variable("W"), split("X", "T"), split("X", "L"), Variable("G"))), List(
      Predicate("<=", List(Variable("X"), Variable("W"))),
      Predicate("pivot", List(Variable("W"), Variable("T"), Variable("L"), Variable("G")))
  ))
  sortKB tell (
    Predicate("pivot", List(Variable("W"), split("X", "T"), Variable("L"), split("X", "G"))), List(
      Predicate(">", List(Variable("X"), Variable("W"))),
      Predicate("pivot", List(Variable("W"), Variable("T"), Variable("L"), Variable("G")))
  ))

  test("append to a list") {
    val l1 = ListBuilder(List(Atom(1), Atom(2), Atom(3)))
    val l2 = ListBuilder(List(Atom(4), Atom(5)))
    val answer = sortKB ask List(Predicate("append", List(l1, l2, Variable("X"))))
    assert(flatten(answer(0)(Variable("X")).asInstanceOf[Predicate]) == List(Atom(1), Atom(2), Atom(3), Atom(4), Atom(5)))
  }

  test("pivot") {
    val l1 = ListBuilder(List(Atom(1), Atom(2), Atom(3), Atom(4)))
    val answer = sortKB ask List(Predicate("pivot", List(Atom(2), l1, Variable("L"), Variable("G"))))
    assert(flatten(answer(0)(Variable("L")).asInstanceOf[Predicate]) == List(Atom(1), Atom(2)))
    assert(flatten(answer(0)(Variable("G")).asInstanceOf[Predicate]) == List(Atom(3), Atom(4)))
  }


  test("sort") {
    val l1 = ListBuilder(List(Atom(3), Atom(2), Atom(1), Atom(4)))
    val answer = sortKB ask List(Predicate("sort", List(l1, Variable("S"))) )
    assert(flatten(answer(0)(Variable("S")).asInstanceOf[Predicate]) == List(Atom(1), Atom(2), Atom(3), Atom(4)))
  }

}
