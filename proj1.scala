trait PList[A] {
  def isEmpty: Boolean
  def +:(x: A): PList[A]
  def tail: PList[A]
  def apply(i: Int): A
  def updated(i: Int, x: A): PList[A]
  def size: Int
  def fill(n: Int, x: A): PList[A]
  def reverse: PList[A]
}

case class Empty[A]() extends PList[A] {
  def isEmpty: Boolean = true
  def +:(x: A): PList[A] = Node(x, None, Empty[(A, A)])
  def tail: PList[A] = throw new UnsupportedOperationException(s"tail of Empty")
  def apply(i: Int): A = throw new IndexOutOfBoundsException(s"apply of Empty")
  def updated(i: Int, x: A): PList[A] = throw new IndexOutOfBoundsException(s"updated of Empty")
  def size: Int = 0
  def fill(n: Int, x: A): PList[A] = {
    if (n == 0) Empty[A]()
    else x +: fill(n - 1, x)
  }
  def reverse: PList[A] = throw new IndexOutOfBoundsException(s"reversed an Empty")
}

case class Node[A](fst: A, snd: Option[A], rest: PList[(A, A)]) extends PList[A] {

  def isEmpty: Boolean = false

  def +:(x: A): PList[A] = snd match {
      case Some(s) => Node(x, None, (fst, s) +: rest)
      case None => Node(x, Some(fst), rest)
    }
  def tail: PList[A] = snd match {
      case Some(s) => Node(s, None, rest)
      case None => // Snd is undefined so we have to drop fst and see if rest is empty or not
        rest match {
          case e :Empty[(A, A)] => Empty[A]()
          case n :Node[(A, A)] => val (f, s) = rest.apply(0)
            Node(f, Some(s), rest.tail)
        }
    }
  def apply(i: Int): A = {
    if (i == 0) fst 
    else tail.apply(i - 1)
  }
  def updated(i: Int, x: A): PList[A] ={
    if (i == 0) this.copy(fst = x)
    else {
    snd match {
      case Some(s1) if i == 1 => this.copy(snd = Some(x))
      case Some(s2) if i >= 2 => // Update in rest.
        val (a, b) = rest.apply((i / 2) - 1)
        val updatedValues = if (i % 2 == 0) (x, b) else (a, x)
        this.copy(rest = rest.updated((i / 2) - 1, updatedValues))
      case None => // Update in rest.
        val (a, b) = rest.apply((i - 1) / 2)
        val updatedValues = if (i % 2 == 0) (a,x) else (x,b)
        this.copy(rest = rest.updated((i - 1) / 2, updatedValues))
      }
    }
  }
  def size: Int = 1 + snd.size + 2 * rest.size
  def fill(n: Int, x: A): PList[A] = Empty[A]().fill(n, x)
  def reverse: PList[A] = reverseHelper(this)
    def reverseHelper(pl: PList[A], rev: PList[A] = Empty[A]()): PList[A] = pl match {
      case Empty() => rev
      case Node(f,s,r) =>
        reverseHelper(pl.tail, pl(0) +: rev)
    }
}

object TestPList {
  def main(args: Array[String]): Unit = {
    def toPList[A](data: List[A]) = {
      val constructedList = data.foldRight(Empty[A](): PList[A]) { // Construct from right using +:
        case (d, acc) => d +: acc
      }
      constructedList
    }
    //_______________________Test Cases__________________________
    val userName = "Joey Benden"
    val dataOdd = (1 to 9).toList
    val dataEven = (1 to 10).toList
    val stringTest = ("Epstein didn't kill himself").toList // 27 spaces
    val stringTestEven =("Epstein didn't kill himself!").toList//28 spaces
    println("                                           ")//spacer for command line
    println(userName)
    println("                                           ")//spacer for command line
    println("ODD PLIST" +"  ==>  " + toPList(dataOdd))
    println("                                           ")//spacer for command line
    println("EVEN PLIST" +"  ==>  " + toPList(dataEven))
    println("                                           ")//spacer for command line
    println("STRING EVEN PLIST" +"  ==>  " + toPList(stringTestEven))
    println("                                           ")//spacer for command line
    println("STRING ODD PLIST" +"  ==>  " + toPList(stringTest))
    println("                                           ")//spacer for command line
    //______________________________CONS TESTS_____________________________________
        val consControl = 99 +: dataOdd
        val consPlist = 99 +: toPList(dataOdd)
        // println(consControl)
        // println(consPlist)
        if(consControl.apply(0)==consPlist.apply(0))  {
        val consControl = 10000000 +: dataOdd
        val consPlist = 10000000 +: toPList(dataOdd)
        // println(consControl)
        // println(consPlist)
        if(consControl.apply(0)==consPlist.apply(0))  {
        val consControl = -6 +: dataOdd
        val consPlist = -6 +: toPList(dataOdd)
        // println(consControl)
        // println(consPlist)
        if(consControl.apply(0)==consPlist.apply(0)){
        val consControl =  1353452+: dataOdd
        val consPlist = 1353452 +: toPList(dataOdd)
        // println(consControl)
        // println(consPlist)
        if(consControl.apply(0)==consPlist.apply(0))  {
        val consControl = -1 +: dataOdd
        val consPlist = -1 +: toPList(dataOdd)
        // println(consControl)
        // println(consPlist)
        if(consControl.apply(0)==consPlist.apply(0))  {
        val consControl = '!' +: stringTest
        val consPlist = '!' +: toPList(stringTest)
        // println(consControl)
        // println(consPlist)
        if(consControl.apply(0)==consPlist.apply(0)){
          println("PASSED ALL TESTS FOR CONS")
        }
        }
        }
        }
        }
        }
      else println("Something is wrong   ==>  CONS")
    //______________________________TAIL TESTS_____________________________________
        val tailControl= dataOdd.tail //odd test
        val tailPList = toPList(dataOdd).tail
        // println(tailControl)
        // println(tailPList)
        if(tailControl.apply(0)==tailPList.apply(0))  {
        val tailControl= dataEven.tail //even test
        val tailPList = toPList(dataEven).tail
        // println(tailControl)
        // println(tailPList)
        if(tailControl.apply(0)==tailPList.apply(0))  {
        val tailControl= stringTest.tail //stringttest
        val tailPList = toPList(stringTest).tail
        // println(tailControl)
        // println(tailPList)
        if(tailControl.apply(0)==tailPList.apply(0)){
        val tailtest2 = List(1,2,3)
        val tailControl= tailtest2.tail
        val tailPList = toPList(tailtest2).tail
        // println(tailControl)
        // println(tailPList)
        if(tailControl.apply(0)==tailPList.apply(0))  {
        val tailtest3 = List(1,2,3,4)
        val tailControl= tailtest3.tail
        val tailPList = toPList(tailtest3).tail
        // println(tailControl)
        // println(tailPList)
        if(tailControl.apply(0)==tailPList.apply(0))  {
        val tailtest4 = (1 to 100).toList
        val tailControl= tailtest4.tail
        val tailPList = toPList(tailtest4).tail
        // println(tailControl)
        // println(tailPList)
        if(tailControl.apply(0)==tailPList.apply(0)){
          println("PASSED ALL TESTS FOR TAIL")
        }
        }
        }
        }
        }
        }
      else println("Something is wrong   ==>  TAIL")
    //______________________________APPLY TESTS_____________________________________
      if(dataOdd.apply(0)==toPList(dataOdd).apply(0)){
        if(dataOdd.apply(1)==toPList(dataOdd).apply(1)){
          if(dataOdd.apply(2)==toPList(dataOdd).apply(2)){ 
            if(dataEven.apply(0)==toPList(dataEven).apply(0)){
              if(dataEven.apply(1)==toPList(dataEven).apply(1)){
                if(dataEven.apply(2)==toPList(dataEven).apply(2)){
                  if(toPList(stringTest)(0) == 'E'){
                  println("PASSED ALL TESTS FOR APPLY")
      }
      }
      }
      }
      }
      }
      }
      else println("Something is wrong   ==>  APPLY")
    //______________________UPDATED TESTS_____________________________________________
                
        val updatedControl = dataOdd.updated(0,99)
        val updatedPlist = toPList(dataOdd).updated(0,99)
        // println(updatedControl)
        // println(updatedPlist)
        if(updatedPlist.apply(0)==updatedControl.apply(0))  {
          val updatedControl = dataOdd.updated(1,24124234)
          val updatedPlist = toPList(dataOdd).updated(1,24124234)
          // println(updatedControl)
          // println(updatedPlist)
        if(updatedPlist.apply(1)==updatedControl.apply(1))  {
          val updatedControl = dataOdd.updated(2,-228534)
          val updatedPlist = toPList(dataOdd).updated(2,-228534)
          // println(updatedControl)
          // println(updatedPlist)
        if(updatedPlist.apply(2)==updatedControl.apply(2)){
        val updatedControl = dataEven.updated(0,99)
        val updatedPlist = toPList(dataEven).updated(0,99)
        // println(updatedControl)
        // println(updatedPlist)
        if(updatedPlist.apply(0)==updatedControl.apply(0))  {
          val updatedControl = dataEven.updated(1,24124234)
          val updatedPlist = toPList(dataEven).updated(1,24124234)
          // println(updatedControl)
          // println(updatedPlist)
        if(updatedPlist.apply(1)==updatedControl.apply(1))  {
          val updatedControl = stringTest.updated(26,'~')
          val updatedPlist = toPList(stringTest).updated(26,'~') //replaces the f w/ ~
          // println(updatedControl)
          // println(updatedPlist)

        if(updatedPlist.apply(2)==updatedControl.apply(2)){
          println("PASSED ALL TESTS FOR UPDATED")
        }
        }
        }
        }
        }
        }
      else println("Something is wrong   ==>  UPDATED")
  //______________________SIZE TESTS_____________________________________________
      val size1 = List(1)
      val size2 = List(1,2)
      val size3 = List(1,2,3)
      val size4 = List(1,2,3,4)
      val size5 = List(1,2,3,4,5)
      val size100 = (1 to 100).toList
      if(dataOdd.size == toPList(dataOdd).size){
        if(dataEven.size == toPList(dataEven).size){
          if(size1.size == toPList(size1).size){
            if(size2.size == toPList(size2).size){
              if(size3.size == toPList(size3).size){
                if(size4.size == toPList(size4).size){      
                 if(size5.size == toPList(size5).size){      
                   if(size100.size == toPList(size100).size){
                          println("PASSED ALL TESTS FOR SIZE")
          }
      }     
      }         
      }      
      }         
      }        
      }
      }
  else println("Something is wrong   ==>  SIZE")
  //______________________FILL TESTS_____________________________________________
    val fill1 = toPList(dataOdd).fill(10,9)// makes a list of 10, 9's
    val fill2 = toPList(dataOdd).fill(100,100)
    val fill3 = toPList(dataOdd).fill(6,99)
    val fill4 = toPList(dataEven).fill(26,'!')
    val fill5 = toPList(dataEven).fill(1000,1)
    val fill6 = toPList(dataEven).fill(17,69)
    if(fill1.size == 10 && fill1.apply(0) == 9){
      if(fill2.size == 100 && fill2.apply(50) == 100){
        if(fill3.size == 6 && fill3.apply(3) == 99){
          if(fill4.size == 26 && fill4.apply(0) == '!'){
            if(fill5.size == 1000 && fill5.apply(669) == 1){
              if(fill6.size == 17 && fill6.apply(9) == 69){
                  println("PASSED ALL TESTS FOR FILL")
                  //println(fill1)
    }
    }
    }
    } 
    }
    }
    else println("Something is wrong   ==>  FILL")
  //______________________REVERSE TESTS__________________________________________
    val reversedODD = toPList(dataOdd).reverse
    val reversedEVEN = toPList(dataEven).reverse
    val reversedString = toPList(stringTest).reverse
    val reversed1 = toPList("racecar".toList).reverse
    val reversed2 = toPList("repaid".toList).reverse// should spell diaper
    if(reversedODD.apply(0) == 9){
      if(reversedEVEN.apply(0) == 10){ 
        if(reversedString.apply(0) == 'f'){
          if(reversed1.apply(0) == 'r'){
            if(reversed2.apply(0) == 'd'){
              println("PASSED ALL TESTS FOR REVERSE")

    }
    }
    }
    }
    }
    else println("Something is wrong   ==>  REVERSE")
  }
}

