package compile
import util.{ CLI, Helper, ErrText }
import ir.{ Atom, IrProgram, Symbol }
import scala.util.parsing.input.Reader
import scala.util.parsing.input.StreamReader
import scala.collection.immutable.PagedSeq
import java.io._
import scala.io.Source
import scala.collection.mutable.{StringBuilder, ListBuffer}
import scala.Console

// Begin parser/scanner imports
import antlr.CommonAST
import antlr.ASTFactory
import antlr.collections.AST
import antlr.Token
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes, DecafScanner, DecafScannerTokenTypes }

import scala.language.postfixOps

import codegen.{ CodeGenerator }

object Compiler {
  val tokenMap = Map(
    DecafScannerTokenTypes.IDENTIFIER -> "IDENTIFIER",
    DecafScannerTokenTypes.INTLITERAL -> "INTLITERAL",
    DecafScannerTokenTypes.CHARLITERAL -> "CHARLITERAL",
    DecafScannerTokenTypes.STRINGLITERAL -> "STRINGLITERAL",
    DecafScannerTokenTypes.TK_true -> "BOOLEANLITERAL",
    DecafScannerTokenTypes.TK_false -> "BOOLEANLITERAL"
  )
  val optnames = Array[String]("cse", "cp", "dce", "ra")

  var outFile: java.io.PrintStream = null

  def main(args: Array[String]): Unit = {
    CLI.parse(args, optnames);
    outFile = if (CLI.outfile == null) Console.out else (new java.io.PrintStream(
      new java.io.FileOutputStream(CLI.outfile)))
    CLI.target match {
      case CLI.Action.SCAN =>
        scan(CLI.infile)
      case CLI.Action.PARSE =>
        System.exit(if (parse(CLI.infile) isEmpty) 1 else 0)
      case CLI.Action.INTER =>
        System.exit(parse(CLI.infile) flatMap semantic(CLI.infile) map ({_ => 0}) getOrElse 1)
      case CLI.Action.ASSEMBLY =>
        System.exit(parse(CLI.infile) flatMap semantic(CLI.infile) map ({_.refract}) map assembly getOrElse 1)
    }
  }

  def assembly(program: lir.Program): Int = {
    if (CLI.debug) {
      println(program.format)
      program.functions.foreach { f =>
        val cfg = f.toCfg
        println("Function: " + f.name + "\n--------")
        println(cfg.format + "\n\n");
      }
    }
    outFile.println(program.toAssembly(CLI.opts.toList, CLI.debug))
    return 0
  }

  def semantic(fileName: String)(tree: CommonAST): Option[IrProgram] = {
    if (CLI.debug) {
      Helper.printAST(tree, 0)
    }
    val program = IrProgram.generate(tree)
    val (g, es) = program.check(ir.Global(ir.ScopeGlobal(), Map[String, ir.Identified](), Map[String, ir.Identified]()))
    val res = if (
      g.getId("main") == Some(ir.Method(None, List[ir.VariableType]())) ||
      g.getId("main") == Some(ir.Method(Some(ir.VariableInt), List[ir.VariableType]())) ||
      g.getId("main") == Some(ir.Method(Some(ir.VariableBoolean), List[ir.VariableType]()))
    ) es else es :+ ir.ErrMsg(ErrText.nomain())(tree)
    if (!res.isEmpty) {
      outFile.println("Semantic errors in file: " + fileName)
      for (e <- res) {
        val (line, column) = Helper.getLocation(e.ref)
        outFile.println("Line: " + line + ":" + column + ": " + e.msg)
      }
    }
    return (if (res isEmpty) Some(program) else None)
  }

  def scan(fileName: String) = {
    try {
      val inputStream: FileInputStream = new java.io.FileInputStream(fileName)
      val scanner = new DecafScanner(new DataInputStream(inputStream))
      scanner.setTrace(CLI.debug)
      var done = false
      while (!done) {
        try {
          val head = scanner.nextToken()
          if (head.getType() == DecafScannerTokenTypes.EOF) {
            done = true
          } else {
            val tokenType = tokenMap.getOrElse(head.getType(), "")
            outFile.println(head.getLine() + (if (tokenType ==  "") "" else " ") + tokenType + " " + head.getText())
          }
        } catch {
          case ex: Exception => {
            Console.err.println(CLI.infile + " " + ex)
            scanner.consume();
          }
        }
      }
    } catch {
      case ex: Exception => Console.err.println(ex)
    }
  }

  def parse(fileName: String): Option[CommonAST] = {
    var inputStream : java.io.FileInputStream = null
    try {
      inputStream = new java.io.FileInputStream(fileName)
    } catch {
      case f: FileNotFoundException => { Console.err.println("File " + fileName + " does not exist"); return null }
    }
    try {
      val scanner = new DecafScanner(new DataInputStream(inputStream))
      val parser = new DecafParser(scanner)
      val factory = new ASTFactory()
      factory.setASTNodeClass(classOf[CommonAST])
      parser.setASTFactory(factory)
      parser.setTrace(CLI.debug)
      parser.program()
      val t = parser.getAST().asInstanceOf[CommonAST]
      if (parser.getError()) {
        println("Parsing error in file: " + fileName)
        println(parser.getErrorMsg())
        None
      } else {
        if (CLI.debug) {
          println(t.toStringList())
        }
        Some(t)
      }
    } catch {
      case e: Exception => Console.err.println(CLI.infile + " " + e)
      None
    }
  }
}
