package Katas

case class BowlingGame(totalRolls: List[Int]) {

  def score: Int = {
    def scoreRecursive(rolls: List[Int] = totalRolls, frame: Int = 1): Int = rolls match {
      case 10::nextRoll::tail if frame <= 9 => 10 + nextRoll + tail.headOption.getOrElse(0) + scoreRecursive(nextRoll::tail, frame + 1)
      case fSpare::sSpare::nextRoll::tail if fSpare + sSpare == 10 && frame <= 9 => 10 + nextRoll + scoreRecursive(nextRoll::tail, frame + 1)
      case first::tail => first + scoreRecursive(tail, frame + 1)
      case _ => 0
    }
    scoreRecursive()
  }

}

object BowlingGame{
  def main(args: Array[String]): Unit = {
    println(BowlingGame(List.fill(12)(10)).score)
  }

}
