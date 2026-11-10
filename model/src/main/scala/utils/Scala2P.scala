package utils

import alice.tuprolog.*

object Scala2P:
  given Conversion[String, Term] = Term.createTerm(_)
  given Conversion[Seq[_], Term] = _.mkString("[", ", ", "]")

  def extractTerm(t: Term, i: Integer): Term =
    t.asInstanceOf[Struct].getArg(i).getTerm

  /** Builds a Prolog engine based on the given theory and exposes it as a function which,
   * given a goal, returns a lazy list containing the solutions.
   */
  def mkPrologEngine(clauses: String*): Term => LazyList[Term] =
    val engine = Prolog()
    engine.setTheory(Theory(clauses mkString " "))
    goal => new Iterable[Term]:
      override def iterator = new Iterator[Term]:
        var solution = engine.solve(goal)
        var allSolutionsExplored = false
        override def hasNext = !allSolutionsExplored && solution.isSuccess
        override def next(): Term =
          val term = solution.getSolution
          if solution.hasOpenAlternatives then solution = engine.solveNext
          else allSolutionsExplored = true
          term
    .to(LazyList)