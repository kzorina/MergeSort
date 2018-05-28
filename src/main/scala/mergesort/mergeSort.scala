package mergesort

import mergesort.my_utils.parallel

import scala.collection.Searching._
import scala.util.Random
object mergeSort {
  val nWorkers = 4
  def mergeIter(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match{
    case(Nil, list2) => list2
    case(list1, Nil) => list1
    case(x :: list1_1, y :: list2_1) =>
      if (x < y) x :: mergeIter(list1_1,list2)
      else y :: mergeIter(list1, list2_1)
  }
  def mergeBinarySearch(list1: List[Int], list2: List[Int]): List[Int] = {
    var result =  List[Int]()
    var pos1 = 0
    for (el <- list1){
      val pos2 = list2.search(el).insertionPoint
      if (pos2 > 0)
        result = result ::: list2.slice(pos1,pos2)

      pos1 = pos2
      result = result :+ el
    }
    result ::: list2.slice(result.length - list1.length,list2.length+list1.length)

  }
  def mergeSortSeq(list: List[Int]): List[Int] = {
    val middle = list.length/2
    if (list.length == 1) list
    else {
      val (left, right) = list splitAt(middle)
      mergeIter(mergeSortSeq(left),mergeSortSeq(right))
    }
  }
  def mergeSortPar(list: List[Int], nWorkers: Int): List[Int] = {

    if (nWorkers > 1) {
      val middle = list.length/2
      lazy val (left, right) = list.splitAt(middle)
      lazy val (l, r ) = parallel(mergeSortPar(left, nWorkers / 2), mergeSortPar(right, nWorkers / 2))
      mergeBinarySearch(l, r)
    }
    else {
      mergeSortSeq(list)
    }

  }
  /*def mergeSortProc_abitparallel(list: List[Int]): List[Int] = {
    val middle = list.length/2
    if (list.length == 1) list
    else {
      val (left, right) = list splitAt(middle)
      val (one, two) = parallel(mergeSortProc(left),mergeSortProc(right))
      mergeIter(one,two)
    }
  }*/


  def main(args: Array[String]): Unit = {
    val array = Array(1,2,6,7,3,4,8,9)
    val list_easy = Array(1,2,6,7,3,4,8,9).toList

    val list = Seq.fill(1000)(Random.nextInt(2000)).toList
    val t0 = System.nanoTime()
    val seqsort = mergeSortSeq(list)
    val t1 = System.nanoTime()
    println(seqsort)
    println("Elapsed time (sequential scan): "+(t1-t0) + " ns")

    val t3 = System.nanoTime()
    val parsort = mergeSortPar(list, nWorkers)
    val t4 = System.nanoTime()
    println(parsort)
    println("Elapsed time (parallel scan): "+(t4-t3) + " ns")

  }
}
