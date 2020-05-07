object hw7 {
  def userName = "Joey Benden"

  type Event = (Int, Int, Char)

  // *** Log*N^2 *** //* is star not multiplication
  def predictions(n: Int, events: List[Event]): List[Char] = {
    val elems = Array.fill(n,3)(new UF_Element())
    var solution: List[Char] = List.empty
    for((first, second, action) <- events) {
      if(action == '1'){
        elems(first)(0) union elems(second)(2)
        elems(first)(1) union elems(second)(0)
        elems(first)(2) union elems(second)(1)
      }
      else if(action == '2'){ // flipped numbers from above
        elems(first)(2) union elems(second)(0)
        elems(first)(0) union elems(second)(1)
        elems(first)(1) union elems(second)(2)
      }
      else if(action == 'P'){ // all the same bc same type
        elems(first)(0) union elems(second)(0)
        elems(first)(1) union elems(second)(1)
        elems(first)(2) union elems(second)(2)
      }
      else{ // question mark case?
        if((elems(first)(0).find() == elems(second)(2).find()) ||
          (elems(first)(1).find() == elems(second)(0).find()) || 
          (elems(first)(2).find() == elems(second)(1).find())){
          solution = solution :+ '1'
        }
        else if((elems(first)(2).find() == elems(second)(0).find()) ||
          (elems(first)(0).find() == elems(second)(1).find()) || 
          (elems(first)(1).find() == elems(second)(2).find())){
            solution = solution :+ '2'
        }
        else if(elems(first)(0).find() == elems(second)(0).find()){
          solution = solution :+ 'P'
        }
        else solution = solution :+ '*'
      }
    }
    return solution
    }


  // because there's only one function in this homework
  // there's no need to be able to turn the tests for that
  // function on or off

  //////////////////////////////////////////////////
  // *** DON'T CHANGE ANYTHING BELOW THIS LINE! ***

  // I've implemented the Union-Find data structure for you.
  // There are three calls you can make:
  //  - new UF_Element
  //      create a new Union-Find element (and set)
  //      initially the new element is the only element
  //      of the new set
  //  - elem1 union elem2
  //      combine the set that elem1 belongs to and
  //      the set that elem2 belongs to into a single set
  //  - elem.find()
  //      the find method gives you a way to test if two elements
  //      belong to the same set by saying
  //         elem1.find() == elem2.find()
  //      if they belong to the same set, this comparison will be true
  //      and if they don't, the comparison will be false
  class UF_Element { // an element in the Union-Find data structure
    private var size = 1
    private var parent: UF_Element = null
    def find(): UF_Element = {
      if (parent == null) this
      else {
        parent = parent.find()
        parent
      }
    }
    def union(other: UF_Element): Unit = {
      val x = find()
      val y = other.find()
      if (x != y) { // if they're equal, then no need to union
        if (x.size < y.size) { // make y the root
          y.size += x.size
          x.parent = y
        }
        else { // make x the root
          x.size += y.size
          y.parent = x
        }
      }
    }
  }

  // There's no need to even look at anything below but you can if you want...

  def main(args: Array[String]): Unit = {
    println("HW7")
    println(userName)
    println("Begin testing predictions")

    val buffer = scala.io.Source.fromFile("hw7.tests")
    var passed = true
    for (Seq(str1,str2,str3) <- buffer.getLines.grouped(3) if passed) {
      val n = str1.toInt
      val events =
        str2.split(" ").grouped(3).toList.map{ case Array(part1,part2,part3) =>
          (part1.toInt, part2.toInt, part3.head) }
      val expected = str3.split(" ").map(_.head).toList
      try {
        val result = predictions(n, events)
        if (result != expected) {
          println("Failed test:")
          println(s"  n = $n")
          println(s"  events = $events")
          println(s"  Returned: $result")
          println(s"  Expected: $expected")
          passed = false
        }
      }
      catch {
        case exn: Exception =>
          println("Failed test:")
          println(s"  n = $n")
          println(s"  events = $events")
          println("Threw exception:")
          println(exn.getMessage)
          exn.printStackTrace()
          passed = false
      }
    }
    buffer.close()
    if (passed) println("Passed all tests")
  }
}
