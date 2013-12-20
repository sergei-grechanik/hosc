package hosc

import HLanguage._
import TermAlgebra._
import hosc.MSG.Generalization
import hosc.MSG.Generalization2

trait HEMaterial {
  def term1: Expression
  def term2: Expression
  
  override def toString: String = this match {
    case HEMCoupling(term1, term2, binders, children) =>
      peelString(term1) + "\n" + peelString(term2) + "\n" + bndStr(binders) + "\n" +
      indent(children.mkString("\n"))
    case HEMDiving(term1, term2, binders, pos, child) =>
      "Dive " + pos + "\n" +
      peelString(term2) + "\n" + bndStr(binders) + "\n" +
      indent(child.toString)
    case HEMFactor(term1, term2, binders) =>
      "Factor" +
      term1 + "\n" + term2 + "\n" + bndStr(binders)
  }
  
  private def bndStr(b: List[(Variable, Variable)]): String =
    b.map{
      case (null,b) => ("null", b.toString)
      case (a,null) => (a.toString, "null")
      case (a,b) => (a.toString, b.toString)
    }.toString
  
  private def peelString(e: Expression): String = e match {
    case _:Variable => e.toString()
    case Constructor(name, args) => name + "/" + args.size + "(...)"
    case LambdaAbstraction(v, t) => "\\ " + v + " -> "
    case Application(_, _) => "@"
    case CaseExpression(_, bs) => "case of " + bs.map(_.pattern).mkString(" ; ")
  }
  
  private def indent(s: String): String =
    "  " + s.replace("\n", "\n  ")
}

case class HEMCoupling(
    term1: Expression, term2: Expression, binders: List[(Variable, Variable)], 
    children: List[HEMaterial]) extends HEMaterial
case class HEMDiving(
    term1: Expression, term2: Expression, binders: List[(Variable, Variable)], 
    pos: Int, child: HEMaterial) extends HEMaterial
case class HEMFactor(
    term1: Expression, term2: Expression, binders: List[(Variable, Variable)]) extends HEMaterial
    
object HEMaterial {
  def he(term1: Expression, term2: Expression): Option[HEMaterial] = he(term1, term2, Nil)

  def heByCoupling(term1: Expression, term2: Expression): Option[HEMaterial] = heByCoupling(term1, term2, Nil)

  private def he(term1: Expression, term2: Expression, binders: List[(Variable, Variable)]): Option[HEMaterial] =
    if(!getFreeVars(term2).exists(i => binders.exists{case (a,b) => a == null && b == i}))
      heByCoupling(term1, term2, binders) orElse
      heByDiving(term1, term2, binders) orElse
      Some(HEMFactor(term1, term2, binders))
    else
      None

  def heByDiving(term1: Expression, term2: Expression, binders: List[(Variable, Variable)]): Option[HEMaterial] = {
    val term1Vars = getFreeVars(term1)
    for ((b1, b2) <- binders) if ((b1 != null) && (term1Vars contains b1)) return None

    term2 match {
      case Constructor(_, args) =>
        for((a,i) <- args.zipWithIndex) he(term1, a, binders) match {
          case None =>
          case Some(h) => return Some(HEMDiving(term1, term2, binders, i, h))
        }
        
        None

      case LambdaAbstraction(v, t) =>
        he(term1, t, (null, v) :: binders).map(HEMDiving(term1, term2, binders, 0, _))

      // subtle point
      case Application(head@Application(_, _), arg) =>
        heByDiving(term1, head, binders).map(HEMDiving(term1, term2, binders, 0, _)) orElse
        he(term1, arg, binders).map(HEMDiving(term1, term2, binders, 1, _))
        
      case Application(head, arg) =>
        he(term1, head, binders).map(HEMDiving(term1, term2, binders, 0, _)) orElse 
        he(term1, arg, binders).map(HEMDiving(term1, term2, binders, 1, _))

      case CaseExpression(sel, bs1) =>
        val bs = bs1 sortWith compareB
        he(term1, sel, binders).map(HEMDiving(term1, term2, binders, 0, _)) orElse {
          for((b,i) <- bs.zipWithIndex) 
            he(term1, b.term, (b.pattern.args map { (null, _) }) ::: binders) match {
              case None =>
              case Some(h) => return Some(HEMDiving(term1, term2, binders, i + 1, h))
            }
          
          None
        }

      case _ => None
    }
  }

  def heByCoupling(term1: Expression, term2: Expression, binders: List[(Variable, Variable)]): Option[HEMaterial] =
    (term1, term2) match {
      case (v1: Variable, v2: Variable) if
          (v1.global == true && v2.global == true && v1.name == v2.name) ||
          (v1.global == false && v2.global == false) &&
          ((binders exists { case (b1, b2) => b1 == v1 && b2 == v2 }) /*|| 
           (binders forall { case (b1, b2) => b1 != v1 && b2 != v2 })*/) =>
        Some(HEMCoupling(term1, term2, binders, Nil))

      case (Constructor(name1, args1), Constructor(name2, args2)) if name1 == name2 =>
        val children = (args1 zip args2).map(args => he(args._1, args._2, binders))
        if(children.forall(_.nonEmpty)) {
          Some(HEMCoupling(term1, term2, binders, children.map(_.get)))
        } else
          None

      case (LambdaAbstraction(v1, t1), LambdaAbstraction(v2, t2)) =>
        he(t1, t2, (v1, v2) :: binders).map(h => HEMCoupling(term1, term2, binders, List(h)))

      // subtle point
      case (a1: Application, a2: Application) =>
        for(h1 <- heByCoupling(a1.head, a2.head, binders); h2 <- he(a1.arg, a2.arg, binders)) 
          yield HEMCoupling(term1, term2, binders, List(h1, h2))

      case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
        val bs1_ = bs1 sortWith compareB
        val bs2_ = bs2 sortWith compareB
        if((bs1_ map (_.pattern.name)) == (bs2_ map (_.pattern.name))) {
          val hsel = he(sel1, sel2, binders)
          val hbs =
            (bs1_ zip bs2_).map(bs => 
              if(bs._1.pattern.name == bs._2.pattern.name)
                he(bs._1.term, bs._2.term, (bs._1.pattern.args zip bs._2.pattern.args) ::: binders)
              else None)
          if(hsel.nonEmpty && hbs.forall(_.nonEmpty)) {
            Some(HEMCoupling(term1, term2, binders, hsel.get :: hbs.map(_.get)))
          } else
            None
        } else
          None
      }

      case _ => None
    }
  
  def toGeneralization(h: HEMaterial): Generalization = {
    val Generalization2(gTerm, dSub) = share(MSG.simplify(toGeneralization2(h)))
    val s1 = dSub.map { case (v, e1, e2) => (v, e1) }
    val s2 = dSub.map { case (v, e1, e2) => (v, e2) }
    
    /*assert(myEquivalent(
        myReduce(applySubstitution(gTerm, s1.toMap)), 
        myReduce(h.term1)))
    assert(myEquivalent(
        myReduce(applySubstitution(gTerm, s2.toMap)), 
        myReduce(h.term2)))*/
    
    Generalization(gTerm, s1, s2)
  }
  
  def toGeneralization2(h: HEMaterial): Generalization2 = h match {
    case HEMFactor(term1, term2, binders) =>
      val v = newVar()
      val free1 = getFreeVars(term1)
      val free2 = getFreeVars(term2)
      val vars = binders.filter(p => free1.contains(p._1) || free2.contains(p._2))
      Generalization2(constructApplication(v, vars.map(_._2)), 
          List((v, 
              constructLambda(vars.map(_._1), term1), 
              constructLambda(vars.map(_._2), term2))))
    case HEMCoupling(term1, term2, binders, children) =>
      val gs = children.map(toGeneralization2(_))
      val ts = gs.map(_.term)
      Generalization2(replaceChildren(term2, ts), gs.flatMap(_.dSub))
    case HEMDiving(term1, term2, binders, pos, child) =>
      toGeneralization2(child) match {
        case Generalization2(v: Variable, (u, t1, t2) :: sub) if v == u && false =>
          val newch =
            for((t,i) <- getChildren(term2).zipWithIndex) yield {
              if(i == pos) t2 else t
            }
          
          Generalization2(v, (u, t1, replaceChildren(term2, newch)) :: sub)
        case Generalization2(
            Application(f, gterm), (g, l1, LambdaAbstraction(v, body)) :: sub) if f == g =>
          val newch =
            for((t,i) <- getChildren(term2).zipWithIndex) yield {
              if(i == pos) body else t
            }
          
          Generalization2(
              Application(f, gterm),
              (g, l1, LambdaAbstraction(v, replaceChildren(term2, newch))) :: sub)
        case Generalization2(gterm, sub) =>
          val f = newVar()
          val v = newVar()
          val newch =
            for((t,i) <- getChildren(term2).zipWithIndex) yield {
              if(i == pos) v else t
            }
          
          Generalization2(
              Application(f, gterm),
              (f, LambdaAbstraction(v, v), LambdaAbstraction(v, replaceChildren(term2, newch))) :: 
                sub)
      }
  }
  
  def replaceChildren(e: Expression, ts: List[Expression]): Expression = e match {
    case _:Variable => e
    case Constructor(name, _) => Constructor(name, ts)
    case LambdaAbstraction(v, _) => LambdaAbstraction(v, ts(0)) 
    case Application(_, _) => Application(ts(0), ts(1))
    case CaseExpression(_, bs1) =>
      val bs = bs1 sortWith compareB
      CaseExpression(ts(0), (bs zip ts.tail).map{ case (b,t) =>  Branch(b.pattern, t)})
  }
  
  def getChildren(e: Expression): List[Expression] = e match {
    case _:Variable => Nil
    case Constructor(name, args) => args
    case LambdaAbstraction(v, t) => List(t) 
    case Application(a, b) => List(a, b)
    case CaseExpression(h, bs1) =>
      val bs = bs1 sortWith compareB
      h :: bs.map(_.term)
  }
  
  def share(g: Generalization2): Generalization2 = g match {
    case Generalization2(_, Nil) => g
    case Generalization2(term, (v, t1, t2) :: sub) =>
      sub.find(s => myEquivalent(s._2, t1) && myEquivalent(s._3, t2)) match {
        case None =>
          val Generalization2(nterm, nsub) =
            share(Generalization2(term, sub))
          Generalization2(nterm, (v, t1, t2) :: nsub)
        case Some((u, e1, e2)) =>
          println(v + " = " + u)
          println(t1 + " = " + e1)
          println(t2 + " = " + e2)
          share(Generalization2(applySubstitution(term, Map(v -> u)), sub))
      }
  }
  
  def myEquivalent(t1: Expression, t2: Expression, bound: List[(Variable, Variable)] = Nil): 
        Boolean = (t1, t2) match {
    case (v1: Variable, v2: Variable) if v1.global == true && v2.global == true =>
      v1.name == v2.name
    case (v1: Variable, v2: Variable) if v1.global == false && v2.global == false =>
      bound.contains((v1, v2)) || v1 == v2
    case (Constructor(name1, args1), Constructor(name2, args2)) if name1 == name2 =>
      ((args1 zip args2) forall (args => myEquivalent(args._1, args._2, bound)))
    case (Application(h1, a1), Application(h2, a2)) => 
      myEquivalent(h1, h2, bound) && myEquivalent(a1, a2, bound)
    case (LambdaAbstraction(b1, v1), LambdaAbstraction(b2, v2)) =>
      myEquivalent(v1, v2, (b1, b2) :: bound)
    case (CaseExpression(sel1, Nil), CaseExpression(sel2, Nil)) =>
      myEquivalent(sel1, sel2, bound)
    case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) if bs1.size == bs2.size => {
      val bs1s = bs1 sortWith compareB
      val bs2s = bs2 sortWith compareB
      if (bs1s.head.pattern.name == bs2s.head.pattern.name){
        myEquivalent(sel1, sel2, bound) && ((bs1s zip bs2s) forall {
          b => 
            (b._1.pattern.name == b._2.pattern.name) &&
            myEquivalent(b._1.term, b._2.term, (b._1.pattern.args zip b._2.pattern.args) ++ bound)
        })
      } else {
        false
      }
    }
    case _ => false
  }
}
