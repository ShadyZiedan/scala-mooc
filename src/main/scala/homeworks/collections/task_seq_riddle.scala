package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {
    @tailrec
    def generate(l: List[Int], done: List[Int] = List()): List[Int] = l match {
      case Nil => done
      case head :: next =>
        val (left, right) = next.span(_ == head)
        generate(right, done ::: List(left.size + 1, head) )
    }
    generate(currentLine)
  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = {
    def generate(l: List[Int]): LazyList[List[Int]] = nextLine(l) #:: generate(nextLine(l))
    List(1) #:: generate(List(1))
  }

}