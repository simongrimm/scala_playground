package Katas

import org.scalatest.{FlatSpec, Matchers}

class BowlingGameTest extends FlatSpec with Matchers {

  behavior of "The score of a Bowling Game"

  "for a empty game (without rolls)" should "be 0" in {
    BowlingGame(List()).score should be (0)
  }
  "for gutters" should "be 0" in {
    BowlingGame(List(0)).score should be (0)
    BowlingGame(List.fill(20)(0)).score should be (0)
  }
  "after the first frame" should "be 10 for a strike or the sum of the first and second roll" in {
    BowlingGame(List(1, 2)).score should be (3)
    BowlingGame(List(5, 5)).score should be (10)
    BowlingGame(List(10)).score should be (10)
  }
  "when no strike or spare occurred" should "be equal to the sum off all rolls" in {
    BowlingGame(List(1, 2, 3)).score should be (6)
    BowlingGame(List(1, 5, 4, 4)).score should be (14)
    BowlingGame(List.fill(20)(1)).score should be (20)
  }
  "when a spare occurred in the first 9 frames" should "count the next roll twice" in {
    BowlingGame(List(9, 1, 3)).score should be (16)
    BowlingGame(List(1, 9, 1, 9, 5, 5, 7)).score should be (10 + 1 + 10 + 5 + 10 + 7 + 7)
    BowlingGame(List(1, 9, 4, 4)).score should be (22)

  }
  "when a strike occurred in the first 9 frames" should "count the next two rolls twice" in {
    BowlingGame(List(10)).score should be (10)
    BowlingGame(List(10,5)).score should be (20)
    BowlingGame(List(10, 10)).score should be (30)
    BowlingGame(List(10, 10, 10)).score should be (10 + 20 + 10 + 10 + 10)
    BowlingGame(List(10, 1, 3)).score should be (18)
    BowlingGame(List(1, 10, 4, 4, 7)).score should be (34)
    BowlingGame(List.fill(18)(1) ++ List(10, 10, 3)).score should be (41)
    BowlingGame(List.fill(10)(10)).score should be (270)
  }
  "when a spare occurred in the 10th frame" should "count the additional roll only once" in {
    BowlingGame(List.fill(18)(1) ++ List(1, 9, 4)).score should be (32)
  }
  "when a strike occurred in the 10th frame" should "count the tow additional rolls only once" in {
    BowlingGame(List.fill(18)(0) ++ List(10, 10, 10)).score should be (30)
    BowlingGame(List.fill(18)(5) ++ List(10, 10, 10)).score should be (8*15 + 10 + 10 + 30)
  }
  "for a perfect game" should "be 300" in {
    BowlingGame(List.fill(12)(10)).score should be (300)
  }
}
