package shisa

import scala.collection.mutable.ListBuffer
import scala.sys.process._

object Exec {
  def execStr(s: String): Result = {
    val buff = new ListBuffer[String]
    val exit = Process(s) ! ProcessLogger(buff += _, buff += _)
    println(s"$s => $exit")
    Result(exit, buff.toList)
  }

  final case class Result(exitCode: Int, lines: List[String])
}
