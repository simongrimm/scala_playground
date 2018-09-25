package ListProblems

import org.scalatest.{FlatSpec, Matchers}
import ListProblems._

class ListProblemsTest extends FlatSpec with Matchers {

  behavior of "solutions to list problems"

  "last" should "return the last element of a list" in {
    last(List(1,2,3)) should be (3)
  }

  it should "return the single element of a list of length 1" in {
    last(List('A')) should be ('A')
  }

  it should "throw NoSuchElementException for an empty list" in {
    an[NoSuchElementException] should be thrownBy( last(Nil))
  }

  "isSorted" should "equal true for a sorted list" in {
    isSorted(List(1,2,3)) shouldEqual(true)
  }

  "isSorted" should "equal false for a unsorted list" in {
    isSorted(List(3,2,3)) shouldEqual(false)
  }

  "isSorted" should "equal true for an empty list" in {
    isSorted(Nil) shouldEqual(true)
  }
}
