package ListProblems

import org.scalatest.{FlatSpec, Matchers}
import ListProblems._

class ListProblemsTest extends FlatSpec with Matchers {

  behavior of "solutions to list problems"

  "last" should "return the last element of a list" in {
    last(List(1,2,3)) should be (3)
    last(List('A')) should be ('A')
  }

  it should "throw NoSuchElementException for an empty list" in {
    an[NoSuchElementException] should be thrownBy last(Nil)
  }

  "lastButOne" should "return last but one element of list of size > 1 " in {
    lastButOne(List(1,2,3)) should be (2)
    lastButOne(List(1,2)) should be (1)
  }

  it should "throw No SuchElement fo a list of size <=1" in {
    an[NoSuchElementException] should be thrownBy lastButOne(Nil)
    an[NoSuchElementException] should be thrownBy lastButOne(List('a'))
  }

  "kthElement" should " return the kth element of a list of size > k >= 0" in {
    kthElement(3, List('A', 'B', 'C', 'D')) should be ('D')
  }
  it should "throw No SuchElement fo a list of size <=k or k < 0" in {
    an[NoSuchElementException] should be thrownBy kthElement(3, List('A', 'B', 'C'))
    an[NoSuchElementException] should be thrownBy kthElement(-1, List('A', 'B', 'C'))
  }

  "isSorted" should "equal true for a sorted list" in {
    isSorted(List(1,2,3)) shouldEqual true
  }

  "isSorted" should "equal false for a unsorted list" in {
    isSorted(List(3,2,3)) shouldEqual false
  }

  "isSorted" should "equal true for an empty list" in {
    isSorted(Nil) shouldEqual true
  }
}
