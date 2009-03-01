package hosc

import scala.util.parsing.input.{CharArrayReader, Reader}
import java.io.{BufferedReader, File, FileReader}

import org.junit.Test
import org.junit.Assert._
import HLanguage._

object TestUtils {
  def termResultFromString(input: String) =
    HParsers0.parseTerm(new CharArrayReader(input.toCharArray))
    
  def typeExprResultFromString(input: String) =
    HParsers0.parseType(new CharArrayReader(input.toCharArray))
    
  def termFromString(input: String) = 
    termResultFromString(input).get
    
  def programResultFromString(input: String) =
    HParsers0.parseProgram(new CharArrayReader(input.toCharArray))
    
  def programResultFromFile(fileName: String) = {
    val file = new File(fileName)
    val sb = new StringBuilder
    val in = new BufferedReader(new FileReader(fileName));
    var str: String = null
    do {
      str = in.readLine
      if (str != null){
        sb.append(str)
        sb.append("\n")
      }
    } while (str != null)
    in.close();
    HParsers0.parseProgram(new CharArrayReader(sb.toString.toCharArray))
  }
  
  def termFromString(input: String, program: Program) = {
    val pr = HParsers0.parseTerm(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    val term = pr.get
    Validator0.valTermWithFreeVars(Set.empty[String] ++ (program.fs map {f => f.name}), term, program)
    val globals = Set[Variable]() ++ (program.fs map (f => Variable(f.name)))
    Postprocessor0.process(term, globals)
    val ti = new TypeInferrer(program.ts)
    //ti.tcTerm(term)
    term
  }
}
