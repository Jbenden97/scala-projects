object hw10 extends App {
  val userName = "Joey Benden"

  //////////////////////////////////////////////////
  // Problem 1: Goody Bags Revisited

  // DON'T FORGET TO INCLUDE BIG-O!
  //O(Nlogn)
  def numGoodyBags(k: Long, numByKind: Array[Long]): Long = {
    // k is the number of goodys that go in each bag
    // numByKind contains the number of items of each different kind
    //   for example, (4,14,7) might mean that you have
    //   4 pencils, 14 lollipops, and 7 whistles
    def helper(bags: Long):Boolean = {
    	var total:Long = 0
    	val predicted:Long = bags * k
    	for(i <- numByKind){
    		if(i >= bags) total += bags
    		else total += i
    	}
    	if(total >= predicted) true
    	else false
    }
    	var lo:Long = 0
    	var hi:Long = numByKind.sum
    	while(hi-lo > 1){
    		val mid:Long =(lo+hi)/2
    		if(helper(mid)==true) lo = mid
    		else hi = mid
    	}
		lo
}
  // change the following flag to true when you're ready to test Problem 1
  val numGoodyBagsReadyToTest = true


  //////////////////////////////////////////////////
  // Problem 2: Lawrence of Arabia

  // DON'T FORGET TO INCLUDE BIG-O!
  def minDays(map: Array[String]): Int = {
    // map is a "picture" of the desert, NOT the kind of action as in list.map

    ???
  }
  // change the following flag to true when you're ready to test Problem 2
  val minDaysReadyToTest = false



  //////////////////////////////////////////////////
  // EVERYTHING BELOW THIS LINE IS RELATED TO TESTING
  // DON'T CHANGE ANY OF IT

  println(userName)

  import scala.util.control.Breaks._

  if (numGoodyBagsReadyToTest) {
    println(">>> Begin testing numGoodyBags")
    val buffer = scala.io.Source.fromFile("hw10a.tests")
    var passed = 0
    var failed = 0
    breakable {
      for (line <- buffer.getLines) {
        val data = line.trim.split(" ").map(_.toLong)
        val (k, numByKind, expectedAnswer) =
          (data.head, data.slice(1,data.length-1), data.last)
        val answer = numGoodyBags(k, numByKind)
        if (answer == expectedAnswer) passed += 1
        else {
          println( "Failed test:")
          println(s"  k = $k")
          println(s"  numByKind = ${numByKind.mkString("(",",",")")}")
          println(s"  Received answer: $answer")
          println(s"  Expected answer: $expectedAnswer")
          failed += 1
          if (failed == 2) {
            println("Aborting remaining tests.")
            break
          }
        }
      }
    }
    buffer.close()
    println(s"Passed $passed tests.")
  }
  else {
    println("*** Skipping tests for numGoodyBags")
  }


  if (minDaysReadyToTest) {
    println(">>> Begin testing minDays")
    val buffer = scala.io.Source.fromFile("hw10b.tests")
    var passed = 0
    var failed = 0
    val input = scala.collection.mutable.ArrayBuffer.empty[String]
    breakable {
      for (line <- buffer.getLines.map(_.trim)) {
        if (line.matches("-?\\d+")) {
          val map = input.toArray
          input.clear()
          val expectedAnswer = line.toInt
          val answer = minDays(map)
          if (answer == expectedAnswer) passed += 1
          else {
            println( "Failed test:")
            println( "  map:")
            for (row <- map) println(s"    $row")
            println(s"  Received answer: $answer")
            println(s"  Expected answer: $expectedAnswer")
            failed += 1
            if (failed == 2) {
              println("Aborting remaining tests.")
              break
            }
          }
        }
        else {
          input += line
        }
      }
    }
    buffer.close()
    println(s"Passed $passed tests.")
  }
  else {
    println("*** Skipping tests for minDays")
  }
}
