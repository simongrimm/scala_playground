/*
https://ide.geeksforgeeks.org/6uDLuJlvUr
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
package Katas

object EncryptThis {

  def encryptThis(text: String): String = text.split(" ").map(encryptWord).mkString(" ")
 
     
  def encryptWord(word: String): String =  word.length match {
    case 0 => ""
    case 1 => word.head.toInt.toString
    case 2 => word.head.toInt.toString + word.last
    case 3 => word.head.toInt.toString + word.last + word.tail.head
    case _ => word.head.toInt.toString + word.last + word.tail.init.tail + word.tail.head
  }

  def main(args: Array[String]) {
    assert(encryptThis("Hello") == "72olle")
    assert(encryptThis("good") == "103doo")
    assert(encryptThis("hello world") == "104olle 119drlo")


    println(encryptThis("Hello"))
    println(encryptThis("hello world"))
    println(encryptThis("good"))
  }

}
