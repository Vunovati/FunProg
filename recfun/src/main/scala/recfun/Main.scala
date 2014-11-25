package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else {
      val c1 = c - 1
      val c2 = c
      pascal(c1, r - 1) + pascal(c2, r - 1)
    }
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def scan(braceStat: Int, index: Int): Boolean = {
      if (braceStat < 0) false
      else if (braceStat > 0 && index == chars.length) false
      else if (braceStat == 0 && index == chars.length) true
      else if (chars(index).toString.equals("(")) scan(braceStat + 1, index + 1)
      else if (chars(index).toString.equals(")")) scan(braceStat - 1, index + 1)
      else scan(braceStat, index + 1)
    }
    scan(0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var numberOfCombinations = 0

    def countCombinations(coinIndex: Int, sum: Int): Unit = {
      if (sum == money) {
        numberOfCombinations = numberOfCombinations + 1
      } else if (sum > money) Nil
      else {
        eachDenominationCount(coinIndex, sum)
      }
    }

    def eachDenominationCount(coinIndex: Int, sum: Int): Unit = {
      if (coinIndex == coins.length) Nil
      else {
        eachDenominationCount(coinIndex + 1, sum)
        countCombinations(coinIndex, sum + coins(coinIndex))
      }
    }

    eachDenominationCount(0, 0)
    numberOfCombinations
  }
}
