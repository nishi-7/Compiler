package com.github.nishi_7

import backend.CompilationEngine
import front.{Lexer, Parser}

import jp.pois.pg4scala.lexer.exceptions.MismatchedCharException

import java.io._
import java.nio.file.Paths
import scala.util.Using

object Compiler {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) throw new IllegalArgumentException("The size of arguments must be one")

    val file = Paths.get(args(0)).toFile
    if (!file.exists()) throw new IllegalArgumentException("The file does not exists")
    fromFile(file)
  }

  def fromFile(f: File): Unit = {
    if (f.isDirectory) {
      f.listFiles().foreach(fromFile)
    } else {
      val fileName = f.toPath.getFileName
      if (!fileName.toString.endsWith(".jack")) return
      if (fileName.toString == ".jack") return
      val absPath = f.toPath.toAbsolutePath.toString
      val out = Paths.get(absPath.substring(0, absPath.lastIndexOf(".")) + ".vm").toFile
      compileFile(f, out)
    }
  }

  def compileFile(in: File, out: File): Unit = {
    if (in.isDirectory) throw new IllegalArgumentException("Input is directory")
    if (out.isDirectory) throw new IllegalArgumentException("Output is directory")

    Using(new BufferedReader(new FileReader(in))) { reader =>
      Using(new BufferedWriter(new FileWriter(out))) { writer =>
        try {
          compile(reader, writer)
        } catch {
          case e: MismatchedCharException => println(e.bufferedString, e.currentChar, e.start, e.end)
        }
        writer.flush()
      }.get
    }.get
  }

  def compile(in: Reader, out: Writer): Unit = {
    val source = Parser.parse(Lexer.lex(in))
    CompilationEngine.write(source, out)
  }
}
