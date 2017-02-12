package scalog.fol

import scala.util.parsing.combinator.RegexParsers

object PrologParser extends RegexParsers {

  def identifier: Parser[String] = """[a-zA-z0-9]+""".r

  def atomicString: Parser[Atom[String]] = """[a-z0-9]+""".r ^^ {
    x => Atom(x)
  }

  def atomicInt: Parser[Atom[Int]] = """[0-9]+""".r ^^ {
    x => Atom(x.toInt)
  }

  def variable: Parser[Variable] = """[A-Z][a-zA-Z0-9]*""".r ^^ {
    x => Variable(x)
  }

  def list: Parser[Term] = """\[""".r ~ terms ~ """\]""".r ^^ {
    case _ ~ list ~ _  => ListBuilder(list)
  }

  def nil: Parser[Atom[String]] = """\[ \]""".r ^^ {x => ListBuilder.End}

  def terms: Parser[List[Term]] = rep(
    term ~ ((", *".r?)) ^^ { case t ~ _ => t }
  )

  def term: Parser[Term] = (split | list | predicate | atomicInt | atomicString | variable | nil)

  def predicate: Parser[Predicate] = identifier ~ """(""" ~ terms ~ """)""" ^^ {
    case functor ~ _ ~ terms ~ _ => Predicate(functor, terms)
  }

  def fact: Parser[Clause] = predicate ~ "." ^^ {
    case f ~ _ => Clause(f, List.empty[Predicate])
  }

  def program: Parser[List[Clause]] = rep(rule | fact)

  def splitCmp: Parser[Predicate] = ( term  ~ "|" ~ term ) ^^ {
    case h ~ _ ~ t => ListBuilder.split(h, t)
  }

  def split: Parser[Predicate] = "[" ~  splitCmp  ~ "]" ^^ {
    case _ ~ x ~ _ => x
  }

  def predicates: Parser[List[Predicate]] = rep(" *".r ~ predicate ~ ",?[\b\n ]*".r ^^ {case _ ~ p ~ _ => p})

  def rule: Parser[Clause] = predicate ~ ":-" ~  predicates ~ "." ^^ {
    case head ~ _ ~ body ~ _ => Clause(head, body)
  }

  def knowledge(progStr: String): Option[KnowledgeBase] = parse(program, progStr) match {
    case Success(clauses, _) => {
      val kb = KnowledgeBase()
      for (clause <- clauses) kb tell clause
      Some(kb)
    }
    case _ => None
  }

  def query(queryStr: String): Option[List[Predicate]] = parse(predicates, queryStr) match {
    case Success(pred, _) => Some(pred)
    case _ => None
  }

  def main(args: Array[String]): Unit = {
    println(parse(fact,"f(mul,z,f(add, x, y))."))
  }

}