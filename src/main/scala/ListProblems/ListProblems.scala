package ListProblems

import scala.annotation.tailrec


object ListProblems {
  @tailrec
  def last[A](as: List[A]): A = as match {
    case Nil => throw new NoSuchElementException
    case head::Nil => head
    case _::tail => last(tail)
  }
  @tailrec
  def lastButOne[A](as: List[A]): A = as match {
    case Nil | _::Nil => throw new NoSuchElementException
    case last_but_one::_::Nil => last_but_one
    case _::tail => lastButOne(tail)
  }
  @tailrec
  def kthElement[A](k: Long, as: List[A]): A = (k, as) match {
    case (_, Nil) => throw new NoSuchElementException
    case (0, head::_) => head
    case (m, _ ::tail) => kthElement(m - 1, tail)

  }
  @tailrec
  def length[A](as: List[A], acc: Long = 0): Long = as match {
    case Nil => 0 + acc
    case _::tail => length(tail, acc+1)
  }

  @tailrec
  def reverse[A](as: List[A], acc: List[A] = Nil): List[A] = as match {
      case Nil => acc
      case head::tail => reverse(tail, head::acc)
  }



  def isSorted[A](as: List[A])(implicit ordering: Ordering[A]): Boolean = {
    import ordering._
    as.sliding(2).forall({case Seq(a,b) => a <= b})
  }



  @tailrec
  def insertIntoSorted[A](a: A, as: List[A], acc: List[A] = Nil)(implicit ordering: Ordering[A]): List[A] = {
    import ordering._
    as match {
      case Nil => acc ::: List(a)
      case head :: tail =>
        assert(tail.isEmpty || head <= tail.head)
        if (a <= head) acc ::: a :: as
        else insertIntoSorted(a, tail, acc ::: head :: Nil)
    }
  }

  def insertionSort[A](as: List[A])(implicit ordering: Ordering[A]): List[A] = as match {
      case Nil => Nil
      case head :: tail => insertIntoSorted(head, insertionSort(tail))
  }



  def mergeTwoSortedLists[A](ls: List[A], rs: List[A])(implicit ordering: Ordering[A]): List[A] = {
    import ordering._
    (ls,rs) match {
      case (_, Nil) => ls
      case (Nil, _) => rs
      case (headL::tailL, headR::tailR) =>
        if(headL <= headR) headL :: mergeTwoSortedLists(tailL, rs)
        else headR :: mergeTwoSortedLists(ls, tailR)
    }
  }

  def mergeSort[A](as: List[A])(implicit ordering: Ordering[A]): List[A] = {
    as match {
      case Nil => as
      case _ =>
        val (left, right) = as.splitAt(as.length / 2)
        mergeTwoSortedLists(left, right)
    }
  }









  def main(args: Array[String]) {
    val l = List('b', 'z', 'D')
    println(s"$l is sorted?: ${isSorted(l)}")
    val l2 = insertIntoSorted[Char]('b', l)
    println(s"$l2 is sorted?: ${isSorted(l2)}")
    val l3 = List(8, 5, 6, 6, 7, 23)
    println(s"$l3 is sorted?: ${isSorted(l3)}")
    val l4 = insertionSort(l3)
    println(s"$l4 is sorted?: ${isSorted(l4)}")
    val l5 = mergeSort(l3)
    println(s"$l5 is sorted?: ${isSorted(l5)}")
    println()
    println(s"last:${last(l2)}")
    println(s"last_but_one:${lastButOne(l2)}")
    println(s"4th element:${kthElement(3, l2)}")
    println(s"length: ${length(l2)}")
    println(s"reverse: ${reverse(l2)}")
  }
}