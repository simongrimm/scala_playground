package ListProblems


object ListProblems {

  //P01
  def last[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException
    case head::Nil => head
    case _::tail => last(tail)
  }

  //P02
  def lastButOne[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException
    case _::Nil  => throw new NoSuchElementException
    case last_but_one::_::Nil => last_but_one
    case _::tail => lastButOne(tail)
  }

  //P03
  def kthElement[A](k: Long, list: List[A]): A = (k, list) match {
    case (_, Nil) => throw new NoSuchElementException
    case (0, head::_) => head
    case (m, _ ::tail) => kthElement(m - 1, tail)

  }

  //P04
  def length[A](list: List[A]): Long = list match {
    case Nil => 0
    case _::Nil => 1
    case _::tail => 1 + length(tail)
  }

  //P05
  def reverse[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case head::last::Nil => last::head::Nil
    case head::tail => reverse(tail):::head::Nil
  }








  type OrderedView[A] = A => Ordered[A]

  def isSorted[A: OrderedView](list: List[A]): Boolean = list.sliding(2).forall(s => s(0) <= s(1))

  def insert[A: OrderedView](new_element: A, list: List[A]): List[A] = list match {
    case Nil => List(new_element)
    case head :: tail => assert(head <= tail.head); if (new_element <= head) new_element :: list else head :: insert(new_element, tail)
  }

  def sort[A: OrderedView](list: List[A]): List[A] = list match {
    case Nil => Nil
    case head :: tail => insert(head, sort(tail))
  }











  def main(args: Array[String]) {
    val l = List('b', 'z', 'D')
    val l2 = insert[Char]('b', l)
    //val l = List(5, 6, 6, 7, 23)
    //val l2 = insert(24, l)
    for (i <- l2)
      print(i + " ")
    println()
    println(s"last:${last(l2)}")
    println(s"last_but_one:${lastButOne(l2)}")
    println(s"4th element:${kthElement(3, l2)}")
    println(s"length: ${length(l2)}")
    println(s"reverse: ${reverse(l2)}")
  }
}