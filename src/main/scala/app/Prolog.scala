package app

import scalog.fol.{PrologParser, Result}

import scala.io.Source

object Prolog {

  def main(args: Array[String]): Unit =
    if(args.size != 2) {
      println("Usage runMain Prolog knowledgebase.pl")
    } else {
      val knowledgeOpt = PrologParser.knowledge(Source.fromFile(args(0)).getLines().mkString("\n").stripMargin)
      println(knowledgeOpt.get.program)
      val queryOpt = PrologParser.query(args(1))
      for {
        kb    <- knowledgeOpt
        query <- queryOpt
      } Result.printResult(kb ask query)
    }

}
