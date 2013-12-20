package hosc

import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.CharArrayReader

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.FileReader
import java.io.FileWriter
import java.io.BufferedReader

import HLanguage._
import Util._

import hosc.sc._
import hosc.exp._

object EqProverApp {
  val help = "usage: hosc.EqProverApp supercompiler input1 input2"
  def main(args : Array[String]) : Unit = {
    args.toList match {
      case scname :: input1 :: input2 :: Nil =>
        val sc = scname match {
	  case "sc1" => SuperCompiler1
	  case "sc2" => SuperCompiler2
	  case "hlsc" => HigherLevelSuperCompiler
	  case "fasthlsc" => FastHLSC
	  case "fullpositive" => FullPositiveSuperCompiler
	  case "naive" => NaiveSuperCompiler
          case "hosc15" => HOSC15
	  case _ => throw new IllegalArgumentException("Unknown supercompiler: " + scname)
	}
	
        val p1 = sc.superCompileFile(input1)
        val p2 = sc.superCompileFile(input2)
        if(Eq.equivalent(p1.goal, p2.goal)) exit(0)
        else exit(1)
      case "-help" :: Nil => 
        println(help)
        return
      case _ => 
        throw new IllegalArgumentException("run hosc.SuperCompilerApp -help for help")       
    }
  }
}
