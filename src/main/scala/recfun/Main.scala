package recfun

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
     /* def loopPasc(c1: Int, r1:Int, acc:Int)= Int{
        if(c1<=1 && r1<=1) acc
        else{
          val a:Int =loopPasc(c1-1,r1-1,acc)
          val b:Int= loopPasc(c1,r1-1,acc)
          acc=loopPasc(c1-1,r1-1,acc)+ loopPasc(c1,r1-1,acc)
        }

      if(r>=0 && c<=r) loopPasc(1,r,c)
      else 0
      */
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      //(if (zero? x) max (/ 1 x))
      if(chars.isEmpty) false
      else {
        def testB(b: Int, ch: List[Char]): Int = {
          if(ch.isEmpty) b
          else if(b<0) b
          else if (ch.head == ')' && b==0) -1
          else{
            if (ch.head == '(') testB(b + 1, ch.tail)
            else if (ch.head == ')' && b>0) testB(b - 1, ch.tail)
            else   testB(b , ch.tail)
            //if(ch.head != '(' && ch.head !=')')
          }
        }
        if (testB(0, chars) == 0) true
        else false
      }
    }
  
  /**
   * Exercise 3
   */
  // suppose that the list begins from the bigger change to the smallest
    def countChange(money: Int, coins: List[Int]): Int = {


          if(money == 0) return 1
          else if (money<0) return 0
          else if (coins.isEmpty) return 0
          else countChange(money,coins.tail) + countChange(money-coins.head,coins)
    }
  }
