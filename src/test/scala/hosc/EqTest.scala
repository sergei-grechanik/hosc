package hosc

import org.junit.Test
import org.junit.Assert._

import Util._

class EqTest {
  /*
  @Test def abs() = testEq("eq/abs1.hs", "eq/abs2.hs")
  @Test def add() = testEq("eq/add1.hs", "eq/add2.hs")
  @Test def app() = testEq("eq/app1.hs", "eq/app2.hs")
  @Test def churchAdd() = testEq("eq/churchAdd1.hs", "eq/churchAdd2.hs")
  @Test def churchMult() = testEq("eq/churchMult1.hs", "eq/churchMult2.hs")
  @Test def churchUnchurch() = testEq("eq/churchUnchurch1.hs", "eq/churchUnchurch2.hs")
  @Test def churchPred() = testEq("eq/churchPred1.hs", "eq/churchPred2.hs")
  @Test def comp() = testEq("eq/comp1.hs", "eq/comp2.hs")
  @Test def filter() = testEq("eq/filter1.hs", "eq/filter2.hs")
  @Test def iter() = testEq("eq/iter1.hs", "eq/iter2.hs")
  @Test def join() = testEq("eq/join1.hs", "eq/join2.hs")
  @Test def lemma() = testEq("eq/lemma1.hs", "eq/lemma2.hs")
  @Test def map_app() = testEq("eq/map_app1.hs", "eq/map_app2.hs")
  @Test def map_comp() = testEq("eq/map_comp1.hs", "eq/map_comp2.hs")
  @Test def oddEven() = testEq("eq/oddEven1.hs", "eq/oddEven2.hs")
  @Test def orEven() = testEq("eq/orEven1.hs", "eq/orEven2.hs")
  @Test def orEvenOdd() = testEq("eq/orEvenOdd1.hs", "eq/orEvenOdd2.hs")
  @Test def pairs() = testEq("eq/pairs1.hs", "eq/pairs2.hs")
  @Test def rep() = testEq("eq/rep1.hs", "eq/rep2.hs")
  @Test def re() = testEq("eq/re1.hs", "eq/re2.hs")
  @Test def mul() = testEq("eq/mul1.hs", "eq/mul2.hs")
  @Test def deepseq() = testEq("eq/deepseq1.hs", "eq/deepseq2.hs")
  @Test def addcomm() = testEq("eq/add-comm1.hs", "eq/add-comm2.hs")*/
  @Test def deepseqaddz() = testEq("eq/deepseq-addz1.hs", "eq/deepseq-addz2.hs")
  
  def testEq(f1: String, f2: String) = {
    println("***")
    println(f1)
    val p1 = supercompile("examples/" + f1)
    val p2 = supercompile("examples/" + f2)
    println(p1.toDocString)
    println(p2.toDocString)
    assertTrue(f1 + " and " + f2 + " should be equivalent", Eq.equivalent(p1.goal, p2.goal))
  }
  
  def supercompile(file: String) = {
    val program = programFromFile(file)
    val sc = new SuperCompiler0(program)
    sc.info = true
    val pt = sc.buildProcessTree(program.goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}
