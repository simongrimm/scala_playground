/*
https://ide.geeksforgeeks.org/ptwYHeKuWg
The goal of this exercise is to convert a string to a new string where each character 
in the new string is '(' if that character appears only once in the original string, 
or ')' if that character appears more than once in the original string. 
Ignore capitalization when determining if a character is a duplicate.Examples:

"din" => "((("
"recede" => "()()()"
"Success" => ")())())"
"(( @" => "))(("
*/

object Solution {
 
 
  def duplicateEncode(word: String): String = {
    word.map(char => word.count(c => char.toLower == c.toLower)).map(wc => { if (wc > 1) ')' else '(' }).mkString
  }

 
  def main(args: Array[String]) {
    println(duplicateEncode("din"))
    println(duplicateEncode("recede"))
    println(duplicateEncode("Success"))
    println(duplicateEncode("(( @"))
  }
}
