package hosc

import HLanguage._
import TermAlgebra._

object HEMSG {

  type Substitution = Tuple2[Variable, Expression]
  type DoubleSubstitution = Tuple3[Variable, Expression, Expression]
  case class Generalization(term: Expression, sub1: List[Substitution], sub2: List[Substitution])
  case class Generalization2(term: Expression, dSub: List[DoubleSubstitution])
  
  def msg(term1: Expression, term2: Expression): Generalization = {
    val Generalization2(gTerm, dSub) = simplify(generalize(term1, term2))
    val s1 = dSub.map { case (v, e1, e2) => (v, e1) }
    val s2 = dSub.map { case (v, e1, e2) => (v, e2) }
    Generalization(gTerm, s1, s2)
  }

  private def generalize(e1: Expression, e2: Expression, bound: List[Variable] = Nil): Generalization2 = {
    null
  }
  
  private def generalizeByCoupling(e1: Expression, e2: Expression, bound: List[Variable] = Nil): Generalization2 = (e1, e2) match {

    case (Variable(n1), Variable(n2)) if n1 == n2 =>
      Generalization2(e1, List())

    case (Constructor(n1, args1), Constructor(n2, args2)) if n1 == n2 => {
      val subR = (args1, args2).zipped.map { generalize(_, _, bound) }
      val genArgs = subR map { _.term }
      val sub = subR.foldRight(List[DoubleSubstitution]()) { _.dSub ++ _ }
      Generalization2(Constructor(n1, genArgs), sub)
    }

    case (LambdaAbstraction(v1, body1), LambdaAbstraction(v2, body2)) => {
      val freshVar = newVar

      val freshBody1 = applySubstitution(body1, Map(v1 -> freshVar))
      val freshBody2 = applySubstitution(body2, Map(v2 -> freshVar))
      val Generalization2(genBody, dSub) = generalize(freshBody1, freshBody2, freshVar :: bound)
      Generalization2(LambdaAbstraction(freshVar, genBody), dSub)  
      
    }

    case (Application(h1, arg1), Application(h2, arg2)) => {
      val Generalization2(genHead, sub1) = generalize(h1, h2)
      val Generalization2(genArg, sub2) = generalize(arg1, arg2)
      Generalization2(Application(genHead, genArg), sub1 ++ sub2)
    }

    case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
      val Generalization2(genSel, sub) = generalize(sel1, sel2)

      // TODO: get rid of it later
      val bs1s = bs1 sortWith compareB
      val bs2s = bs2 sortWith compareB

      if (bs1s(0).pattern.name == bs2s(0).pattern.name) {
        
      // fresh branches
        val freshBs: List[(Pattern, Expression, Expression)] =
          (bs1s, bs2s).zipped.map { (b1, b2) =>
            val freshPatVars = b1.pattern.args map {_ => newVar}
            val freshPat = Pattern(b1.pattern.name, freshPatVars)
            val freshB1 = applySubstitution(b1.term, Map(b1.pattern.args zip freshPatVars: _*))
            val freshB2 = applySubstitution(b2.term, Map(b2.pattern.args zip freshPatVars: _*))
            (freshPat, freshB1, freshB2)
        }

        // generalized branches
        val genBs: List[(Pattern, Generalization2)] =
          freshBs map { case (p, t1, t2) => (p, generalize(t1, t2)) }

        val genCase = CaseExpression(genSel, genBs map { case (p, Generalization2(t, _)) => Branch(p, t) })
        val genSub = genBs.map { _._2 }.foldRight(sub) { _.dSub ++ _ }
      
        val boundVars = (genBs map {case (p, g) => p.args}).flatten
      
        if (isValidSub(boundVars, genSub)) {
          Generalization2(genCase, genSub)
        } else {
        trivialGen(e1, e2)
        }
      
      } else {
      trivialGen(e1, e2)
      }

    }

    case _ =>
      trivialGen(e1, e2)

  }
  
  private def trivialGen(e1: Expression, e2: Expression) = {
    val nv = newVar
    Generalization2(nv, List((nv, e1, e2)))
  }

  private def simplify(gen2: Generalization2): Generalization2 = {
    gen2.dSub match {
      case Nil => gen2
      case (el@(v, e1, e2)) :: els => {
        val Generalization2(simpledTerm, simpledSub) = simplify(Generalization2(gen2.term, els))
        val (same, other) = simpledSub partition { case (_, t1, t2) => e1 == t1 && e2 == t2 }
        val sub = Map(same map { case (v1, _, _) => (v1, v) }: _*)
        val term = applySubstitution(simpledTerm, sub)
        Generalization2(term, el :: other)
      }
    }
  }
  
  private def isValidSub(boundVars: List[Variable], sub: List[DoubleSubstitution]) =
    sub forall {isValidSubElem(boundVars, _)}
  
  private def isValidSubElem(boundVars: List[Variable], dSub: DoubleSubstitution): Boolean = dSub match {
    case (_, e1, e2) => {
      val domainVars = getFreeVars(e1) ++ getFreeVars(e2)
      boundVars forall {!domainVars.contains(_)}
    }
  }

}
