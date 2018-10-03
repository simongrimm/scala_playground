/*
https://ide.geeksforgeeks.org/2XbfPnWsQ7
Task
Given a positive integral number n, return a strictly increasing sequence (list/array/string depending on the language) of numbers, so that the sum of the squares is equal to n².

If there are multiple solutions (and there will be), return the result with the largest possible values:

Examples
decompose(11) must return [1,2,4,10]. Note that there are actually two ways to decompose 11², 11² = 121 = 1 + 4 + 16 + 100 = 1² + 2² + 4² + 10² but don't return [2,6,9], since 9 is smaller than 10.

For decompose(50) don't return [1, 1, 4, 9, 49] but [1, 3, 5, 8, 49] since [1, 1, 4, 9, 49] doesn't form a strictly increasing sequence.

Note
Neither [n] nor [1,1,1,…,1] are valid solutions. If no valid solution exists, return nil, null, Nothing, None (depending on the language) or "[]" (C) ,{} (C++), [] (Swift).

The function "decompose" will take a positive integer n and return the decomposition of N = n² as:

[x1 ... xk] or
"x1 ... xk" or
Just [x1 ... xk] or
Some [x1 ... xk] or
{x1 ... xk} or
"[x1,x2, ... ,xk]"
depending on the language (see "Sample tests")
 */

package Katas

import scala.annotation.tailrec

object SquareSquares {


  def decompose(n: Long): String = {
    val nSquared: Long = n*n

    @tailrec
    def decomposeRecursive(k: Long, ks: List[Long]): List[Long] = {
      val kSquared = k*k
      val sumSquared = ks.map(l => l*l).foldLeft(0L)((a,b) => a+b)
      k match {
        case 1L if ks.isEmpty & kSquared + sumSquared != nSquared => Nil
        case 1L if kSquared + sumSquared != nSquared  =>  decomposeRecursive(ks.head - 1L, ks.tail)
        case _ if kSquared + sumSquared < nSquared => decomposeRecursive(k - 1L, k::ks)
        case _ if kSquared + sumSquared > nSquared => decomposeRecursive(k - 1L, ks)
        case _  => k::ks
      }
    }
    val decomposition = decomposeRecursive(n - 1L, Nil)
    if(decomposition.isEmpty) null else decomposition.mkString(" ")

  }

  def main(args: Array[String]): Unit = {

    println(decompose(709185))
    println(decompose(9927447))
    assert(decompose(709185) == "3 12 46 1190 709184")
    assert(decompose(754411) == "2 7 28 1228 754410")
    assert(decompose(258900) == "1 2 7 28 719 258899")
    assert(decompose(415475) == "2 32 911 415474")
    assert(decompose(518452) == "1 7 23 1018 518451")
    assert(decompose(522853) == "1 8 34 1022 522852")
    assert(decompose(72217) == "1 2 3 7 27 379 72216")
    assert(decompose(242848) == "1 2 7 35 696 242847")
    assert(decompose(500596) == "1 3 5 34 1000 500595")

  }


}
