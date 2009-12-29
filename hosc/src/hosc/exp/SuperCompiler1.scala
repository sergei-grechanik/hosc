package hosc.exp

import hosc.HLanguage._
import hosc.HE._
import hosc.MSG._
import hosc.TermAlgebra._
import hosc.ProcessTree._
import hosc.LangUtils._

class SuperCompiler1(program: Program){
  val emptyMap = Map[Variable, Expression]()
  val debug = false
  val useControl = true
  
  def driveExp(expr: Expression): List[Pair[Expression, Map[Variable, Expression]]] = expr match {
    case LetExpression(bs, t) => {
      (t, Map[Variable, Expression](bs map {p => (p._1, p._2)} :_*)) :: 
        (bs map {b => (b._2, emptyMap)}) 
    }
    case t => decompose(t) match {
      case ObservableVar(_) => Nil
      case ObservableCon(c) => c.args map {a => (a, emptyMap)}
      case ObservableVarApp(_, app) => extractAppArgs(app) map {a => (a, emptyMap)}
      case ObservableLam(l) => (l.t, emptyMap) :: Nil
      case context: Context => context.redex match {
        case RedexCall(v) => {
          val lam = program.getFunction(v.name).get.lam
          (context.replaceHole(freshBinders(lam)), emptyMap) :: Nil 
        }
        case RedexLamApp(lam, app) => 
          (context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg))), emptyMap) :: Nil
        case RedexCaseCon(c, ce) => {
          val b = ce.branches.find(_.pattern.name == c.name).get
          val sub = Map[Variable, Expression]() ++ (b.pattern.args zip c.args)
          (context.replaceHole(applySubstitution(b.term, sub)), emptyMap) :: Nil          
        }
        case RedexCaseVar(v, CaseExpression(sel, bs)) =>
          (sel, emptyMap) :: (bs map 
            {b => (replaceTerm(context.replaceHole(b.term), v, Constructor(b.pattern.name, b.pattern.args)), Map(v-> Constructor(b.pattern.name, b.pattern.args)))})  
        case RedexCaseVarApp(a, CaseExpression(sel, bs)) =>
          (sel, emptyMap) :: (bs map 
            {b => (replaceTerm(context.replaceHole(b.term), a, Constructor(b.pattern.name, b.pattern.args)), emptyMap)})
      }
    }
  }  
  
  def buildProcessTree(e: Expression): ProcessTree = {
    val p = ProcessTree(e)
    if (debug) {
      println(program.toDocString)
    }
    while (!p.isClosed) {
      if (debug) { 
        println(p)
        println("==========")
      }
      val beta = p.leafs.find(!_.isProcessed).get
      val bExpr = beta.expr
      beta.expr match {
        case LetExpression(_, _) => drive(p, beta)
        case bTerm if canBeEnhanced_?(bTerm) => {
          beta.ancestors find equivalenceTest(beta) match {
            case Some(alpha) => beta.repeatedOf = alpha; 
            case None => {
              beta.ancestors find instanceTest(beta) match {
                case Some(alpha1) => makeAbstraction(p, beta, alpha1) 
                case None => { 
                  beta.ancestors find heByCouplingTest(beta) match {
                    case Some(alpha) => {
                      if (debug) {
                        println("GENERALIZATION FROM SC0")
                      }
                      makeAbstraction(p, alpha, beta)
                    }
                    case None => drive(p, beta)
                  }
                }
              }
            }
          }
        }
        case _ => drive(p, beta)
      }      
    }
    renameVars(p)
  }
  
  private def instanceTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => sameRedex(aTerm, bNode.expr) && instanceOf(aTerm, bNode.expr) && checkControl(aNode, bNode);
  }
  
  private def equivalenceTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => equivalent(aTerm, bNode.expr) && checkControl(aNode, bNode);
  }
  
  
  private def heByCouplingTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm if (sameRedex(aTerm, bNode.expr) && heByCoupling(aTerm, bNode.expr)) && checkControl(aNode, bNode) => { 
      //return true;
      val sca = sc(aTerm);
      val scb = sc(bNode.expr);
      val r = HE1.heByCoupling(sca, scb)
      println(r)
      println(format(canonize(aTerm)))
      println("------")
      println(format(canonize(bNode.expr)))
      println("------")
      
      println(format(sca))
      println("------")
      println(format(scb))
      println("------")
      println("------")
      
      
      r
    };
    case _ => false
  }
  
  /*
  private def heByCouplingTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => sameRedex(aTerm, bNode.expr) && heByCoupling(aTerm, bNode.expr) && checkControl(aNode, bNode);
  }*/
  
  private def checkControl(aNode: Node, bNode: Node): Boolean = {
    if (!useControl) {
      return true
    }
    if (isGlobal(bNode)) {
      true
    } else {
      val nodesBetween = bNode.ancestors.takeWhile(_ != aNode)
      nodesBetween.forall(!isGlobal(_))
    }
  }
  
  private def isGlobal(n: Node): Boolean = n.expr match {
    case LetExpression(_, _) => false
    case e => decompose(e) match {
      case c: Context => c.redex match {
        case RedexCaseVar(_, _) => true
        case RedexCaseVarApp(_, _) => true
        case _ => false
      }
      case _ => false
    }
  }
  
  private def sc(expr: Expression): Expression = {
    val sc0 = new SuperCompiler(program)
    val pt = sc0.buildProcessTree(expr)
    new CodeConstructor(program, pt, true).generateProgram().goal
  }
  
  def canBeEnhanced_?(t: Expression) = decompose(t) match {
    case c@ContextHole(_) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case r: RedexCaseVarApp => true
      case _ => false
    }
    case c@ContextApp(_, _) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case r: RedexCaseVarApp => true
      case _ => false
    }
    case c@ContextCase(_, _) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case r: RedexCaseVarApp => true
      case _ => false
    }
    case _ => false
  }
  
  def canBeEnhanced1_?(t: Expression) = decompose(t) match {
    case c@ContextCase(_, _) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case r: RedexCaseVarApp => true
      case _ => false
    }
    case _ => false
  }
  
  def sameRedex(t1: Expression, t2: Expression) : Boolean = (decompose(t1), decompose(t2)) match {
    case (c1: Context, c2: Context) => true//c1.redex.getClass() == c2.redex.getClass()
    case _ => false
  }
  
  def drive(t: ProcessTree, n: Node): Unit = {
    t.addChildren(n, driveExp(n.expr))
  }
  
  def makeAbstraction(t: ProcessTree, alpha: Node, beta: Node): Unit = {
    val aTerm = alpha.expr
    val bTerm = beta.expr
    val g = msg(aTerm, bTerm)
    if (g.sub1.isEmpty){
      t.replace(alpha, g.term)
    } else {
      if (debug){
        println(format(canonize(aTerm)))
        println(format(canonize(bTerm)))
      }
      var term = g.term
      var subs = g.sub1
      if (debug) {
        println(format((LetExpression(g.sub1, g.term))))
      }
      t.replace(alpha, LetExpression(g.sub1, g.term))
    }    
  }
  
  def renameVars(p: ProcessTree): ProcessTree = {
    val vars = p.rootNode.getAllVars()
    var i = 0
    def createVar(): Variable = {      
      var nv: Variable = null
      do {
        nv = varFor(i)
        i += 1
      } while (vars contains nv)
      nv
    }
    var map = Map[Variable, Variable]()
    for (v <- vars.toList) {
      if (isSynthetic(v)) {
        map = map + (v -> createVar)
      }
    }
    p.rootNode sub map
    p
  }
  
  private val vNames = "xyzuvwprst".toArray
  
  private def varFor(j: Int) = {
    if (j <= 9) 
      Variable("" + vNames(j))
    else 
      Variable("" + vNames(j % 10) + Integer.toString(j / 10))   
  }
  
  private def isSynthetic(v: Variable) = v.name startsWith "$" 
  
}