package shisa

import scala.collection.mutable.ListBuffer
import scala.sys.process._

object Exec {
  def execStr(s: String): Result = {
    val buff = new ListBuffer[String]
    Result(Process(s) ! ProcessLogger(buff += _, buff += _), buff.toList)
  }

  final case class Result(exitCode: Int, lines: List[String])
}
