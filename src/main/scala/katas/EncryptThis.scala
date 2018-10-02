/*
https://ide.geeksforgeeks.org/VGWJSVEQHs
You want to create secret messages Here are the conditions:

Your message is a string containing space separated words.
You need to encrypt each word in the message using the following rules:
The first letter needs to be converted to its ASCII code.
The second letter needs to be switched with the last letter
Keepin' it simple: There are no special characters in input.

Examples:encryptThis "Hello" == "72olle"
encryptThis "good" == "103doo"
encryptThis "hello world" == "104olle 119drlo"
*/
object EncryptThis {

  def encryptThis(text: String): String = text.split(" ").map(encryptWord).mkString(" ")
 
     
  def encryptWord(word: String): String =  word.length match {
      case 0 => ""
      case 1 => word.head.toInt + ""
      case 2 => word.head.toInt + word.tail.last.toString
      case 3 => word.head.toInt + word.tail.last.toString + word.tail.head.toString
      case _ => word.head.toInt + word.tail.last.toString + word.tail.init.tail.toString + word.tail.head.toString
  }

  def main(args: Array[String]) {
      println(encryptThis("Hello"))
      println(encryptThis("hello world"))
      println(encryptThis("good"))
  }

}
