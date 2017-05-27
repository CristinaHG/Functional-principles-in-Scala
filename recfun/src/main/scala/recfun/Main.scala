package recfun

import sun.font.TrueTypeFont

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("balanceo de paréntesis")
    print(balance("(if (zero? x) max (/ 1 x))".toList))
    print(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    print(balance(":-)".toList))
    print(balance("())(".toList))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c==0 || r==c) 1
      else pascal(c-1,r-1)+pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balanceSubstring(i:Int, j:Int,chars: List[Char]): Boolean ={

        var substring=chars.slice(i,j)
        substring=substring.filter(p => (p =='(' || p== ')'))
        if(!substring.isEmpty) {
          if (substring.last == ')') substring = substring.tail.dropRight(1)
          else substring=substring.dropRight(1)
        }

        if (chars.isEmpty) true
        else if (chars.size==1 && (chars.head=='(' || chars.head==')')) false
        else if (chars.lastIndexOf('(')<0 && (chars.lastIndexOf(')')>0) || (chars.lastIndexOf(')')<0 && (chars.lastIndexOf('(')>0))) false
        else {
          var newcharList=chars.slice(0,i)++substring
          balanceSubstring(newcharList.lastIndexOf('('),newcharList.length,newcharList)
        }
      }

      balanceSubstring(chars.lastIndexOf('('),chars.length,chars)
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money<0 || coins.isEmpty) 0
      else if (money==0) 1

      else countChange(money-coins.head,coins) +countChange(money,coins.tail)
    }
  }
