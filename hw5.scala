object hw5 extends eecs.cs385 {
  def userName = "Joey Benden"

  // Sherlock with memoization
  // <O(N^2)> filling the 2D array 
  def predictionMemo(money: Array[Int]): (Int,Int) = {
    val memo = Array.fill(money.length+1,money.length+1)((-99,-99))
    def play(lo:Int, hi:Int):(Int,Int)={
      if(memo(lo)(hi)._1 == -99) memo(lo)(hi) = {
      if (lo == hi) (0,0)
      else{
        val (theirs1, mine1) = play(lo+1,hi)
        val (theirs2, mine2) = play(lo, hi-1)
        if(mine1+money(lo) > mine2 + money(hi-1))
          (mine1+money(lo), theirs1)
        else (mine2+money(hi-1), theirs2)
      }
      }
      memo(lo)(hi)
    }
    play(0,money.length)
  }
  test("predictionMemo", predictionMemo _, "money")

  // Sherlock with dynamic programming
  // <FILL IN BIG-O HERE> n^2
  def predictionDP(money: Array[Int]): (Int,Int) = {
    val play = Array.fill(2,money.length)((0,0))
    //def play(lo:Int, hi:Int):(Int,Int)={
      //if(memo(lo)(hi)._1 == -99) memo(lo)(hi) = {
    for(lo <- money.length to 0 by -1; hi <- lo to money.length-1) play(lo)(hi) = {
      if (lo == hi) (money(lo),0)
      else{
        val (theirs1, mine1) = play((lo+1)%2,hi)
        val (theirs2, mine2) = play(lo %2, hi-1)
        if(mine1+money(lo) > mine2 + money(hi-1))
          (mine1+money(lo), theirs1)
        else (mine2+money(hi-1), theirs2)
      }
    }
      //memo(lo)(hi)

    if(money.isEmpty) (0,0)
    play(0)(money.length)
  }
  test("predictionDP", predictionDP _, "money")

  //////////////////////////////////////////////////

  // Cell phone towers with memoization
  // <O(N'^2)> fill the table is n and the work for each spot in table is n mAKING n^2
  def totalCostMemo(restStops: Array[Int], cost: Int => Int): Int = {
    // cost is a function, call it like cost(5)
    val memo = Array.fill(restStops.length+1)(-99)
    def best(i:Int): Int = {
      if (memo(i) == -99) memo(i)={
      if(i == restStops.length) 0
      else{
        var minCost = Int.MaxValue
        for(j <- i until restStops.length){
          minCost = minCost min cost(restStops(j)-restStops(i)) + best(j+1)
        }
        minCost
      }
    }
    memo(i)
    }
    best(0)
  }
  // the "use" function is used by the tester, don't call it yourself
  // also, notice that there are two separate sets of tests for this problem
  def useMemo(cost: Int => Int): Array[Int] => Int = totalCostMemo(_,cost)
  test("totalCostMemo with cost(d) = 100 + d",useMemo(d => 100 + d),"restStops")
  test("totalCostMemo with cost(d) = 100 + d*d",useMemo(d => 100 + d*d),"restStops")

  // Cell phone towers with dynamic programming
  // <FILL IN BIG-O HERE>
  def totalCostDP(restStops: Array[Int], cost: Int => Int): Int = {
    ???
  //   val best = Array.fill(restStops.length+1)(0)
  //   //def best(i:Int): Int = {
  //     //if (memo(i) == -99) memo(i)={
  //     for(i <- restStops.length to 0 by -1) best(i) ={
  //     //if(i == restStops.length) 0
  //     //else{
  //       var minCost = Int.MaxValue
  //       for(j <- i until restStops.length){
  //         minCost = minCost min cost(restStops(j)-restStops(i)) + best(j+1)
  //       }
  //       minCost
  //     //}
  //   }
    
  //   best(0)
   }
  // the "use" function is used by the tester, don't call it yourself
  // also, notice that there are two separate sets of tests for this problem
  def useDP(cost: Int => Int): Array[Int] => Int = totalCostDP(_,cost)
  ignoretest("totalCostDP with cost(d) = 100 + d",useDP(d => 100 + d),"restStops")
  ignoretest("totalCostDP with cost(d) = 100 + d*d",useDP(d => 100 + d*d),"restStops")

  //////////////////////////////////////////////////

  // Sherlock with dynamic programming and a smaller table
  // <FILL IN BIG-O HERE>
  def predictionDPSmaller(money: Array[Int]): (Int,Int) = {
    
  }
  ignoretest("predictionDPSmaller", predictionDPSmaller _, "money")

}
