// __JHB__ I DID NOT USE ANY SOURCES OR ASSISTANCE REQUIRING
// DOCUMENTATION IN COMPLETING THIS ASSIGNMENT.

object hw6 {
  val userName = "Joey Benden"
  import scala.language.postfixOps
  // as you finish each method, change the false to true
  val consReadyToTest = true // cons is +:
  val tailReadyToTest = true
  val applyReadyToTest = true
  val updatedReadyToTest = true
  val fastSizeReadyToTest = true
  val fillReadyToTest = false

  sealed trait FPTree { // Ford Prefect Trees
    // abstract methods, will be implemented in Node and Empty
    def isEmpty: Boolean
    def size: Int
    def +:(x: Int): FPTree
    def tail: FPTree
    def apply(i: Int): Int
    def updated(i: Int, x: Int): FPTree
    def fastSize: Int

    // concrete methods, you can use these but don't need to implement them
    def head: Int = apply(0)
    def draw(leftPadding: Int = 4): String = {
      // draws a picture of the tree, GREAT FOR DEBUGGING!
      // print the resulting string to see the picture
      val box = makeBox(this)
      box.lines.map(spaces(leftPadding) + _ + "\n").mkString
    }
  }

  case object Empty extends FPTree { // I've implemented all of these for you
    def isEmpty: Boolean = true
    def size: Int = 0

    def +:(x: Int): FPTree = Node(x,Empty,Empty)
    def tail: FPTree = throw new UnsupportedOperationException("tail of Empty")
    def apply(i: Int): Int = throw new IndexOutOfBoundsException(s"apply of Empty")
    def updated(i: Int, x: Int): FPTree = throw new IndexOutOfBoundsException(s"updated of Empty")

    def fastSize: Int = 0
  }

  case class Node(item: Int, left: FPTree, right:FPTree) extends FPTree {

    def isEmpty: Boolean = false
    def size: Int = 1 + left.size + right.size

    // fill in the logic for the following methods
    def +:(x: Int): FPTree = {
      Node(x,right.+:(item),left)
    }
   // def tail: FPTree = {
   //  val l:Node= left.asInstanceOf[Node]
   //  Node(l.item,right,l.left)
   // }
      def tail: FPTree = left match{
        case Empty => val empty:FPTree = Empty; empty
        case Node(i,l,r) => Node(i,right,left.tail)
        }
        //Node(left.item, right, left.left)
       //Node(apply(1),right,Node(apply(2),left,right))
      // if(left.isEmpty){
      //   Node(99,Empty,Empty)
      // }

      // else{
      // val l:Node = left.asInstanceOf[Node]
      // val x:FPTree = Node(l.item,right,l.left).asInstanceOf[FPTree]
      // x
      
     // }
   //}
      //
    //   Node(i.item,left,right)
    // }

    def apply(i: Int): Int ={
      //var index = i
      if(i == 0) item
      else if (i % 2 == 1){
        left.apply(i/2)
      }
      else{
        right.apply((i/2)-1)
      }
    }

    //THIS  WORKS
    // def apply(i: Int): Int ={
    //   var index:Int = 1
    //   var ltail = tail
    //   if(i == 0) item
    //   else{
    //         while(index != i){
    //           ltail=ltail.tail
    //           index+=1
    //         }
    //      ltail.head
    // }
    // }
  



    //   else{
    //       if(i %2 == 1){
    //         while(index != i){
    //           ltail=ltail.tail
    //           index+=1
    //         }
    //      ltail.head
    //       }
    //       else{
    //         while(index != i){
    //           rtail=rtail.tail
    //           index+=1
    //         }
    //       rtail.head
    //   }
    //   }
    // }
    

      // else{
      //   if(index % 2 == 1){
      //     left.tail
      //   }

    //   if(index % 2 == 1) = left match {
    //     case Empty => 0
    //     case Node(it,l,r) => Node 
    //   }
    // }
      //i match{
    // case Empty => null
    // case Node(it,l,r) if(i == 0) =>  it
    // case Node(it,l,r) if(i % 2 == 1) => 
    // case Node(it,l,r) if(i % 2 == 0) =>
    // }
    //   }
    //   else if(index % 2 == 1){
    //     index+=1
    //     if(l.item!= index){
    //       l
    //     }

    //   }

    // }
      //{
    //   var count = 0
    //   var level =0
    //   if (i == 0) item
    //   if(i % 2 == 1){
    //     while(i!= count){
    //      left, count+=1
    //      level+=1
    //      if(2^level+1 ==right) right
    //      else left
    //     
 
    //}
    //  

    //   }
    // }
    //   if(i%2 == 1){
     def updated(i: Int, x: Int): FPTree = {
        if(i == 0) Node(x,left,right)
        else if (i % 2 == 1){
          Node(item,left.updated(i/2,x),right)

        }
        else{
          Node(item,left,right.updated((i/2)-1,x))
        }
      }
       
    //{
//       apply(i) = x
// }
      //i match{
    //   case Empty => val empty:FPTree = Empty; empty
    //   case 0 => Node(x,left,right)
    //   case Node(it,l,r) => val xret = x ; Node(xret,left,right)

    // } //{
    //   var index:Int = 1
    //   var ltail = tail
    //   var rtail = tail
    //   if(i == 0) Node(x,left,right)
    //   else{
    //       if(i %2 == 1){
    //         while(index != i){
    //           ltail=ltail.tail
    //           index+=1
    //         }
    //         ltail.head
    //       }
    //       else{
    //         while(index != i){
    //           rtail=rtail.tail
    //           index+=1
    //         }
    //       rtail.head
    //   }
    //   }
    // }

    def fastSize: Int ={
      def count(tree:FPTree): Int = tree match {
      case Empty => 0
      case Node(i,l,r) => 1+count(l)+count(r)
        }
        count(Node(item,left,right))
      }

// EXTRA CREDIT, DON'T FORGET BIG-O
// O(n)
  }

  object FPTree { // companion object
    val empty: FPTree = Empty

    def fill(n: Int)(x: Int): FPTree = ??? //{
      //var tre: FPTree = Empty
    //   for(i <- 0 until n) {
    //     empty.+:(x)
    //   }
    //   empty
    // }
     // EXTRA CREDIT, DON'T FORGET BIG-O
  }

  ////////////////////////////////////////////////////////////////
  // *** DO NOT CHANGE OR DELETE ANYTHING BELOW THIS LINE ***
  // There's no need to even look at it but you can if you want...

  def main(args: Array[String]): Unit = {
    println("HW6")
    println(userName)

    for ((name,test,ready) <- allTests) {
      if (ready) {
        println(s"Begin testing $name")
        var flag = true
        for (input <- allInputs if flag) {
          try {
            test(input).foreach{ errMsg =>
              flag = false
              print(errMsg)
            }
          }
          catch {
            case exn: Exception =>
              println("  Exception thrown for input $input")
              flag = false
              println(exn.getMessage())
              exn.printStackTrace()
          }
        }
        if (flag) println(s"  Passed all tests for $name")
        else println(s"  Skipping remaining tests for $name")
      }
      else println(s"*** Ignoring tests for $name")
    }
  }

  val allTests: List[(String,List[Int] => Option[String],Boolean)] =
    List(("+:", testCons _, consReadyToTest),
         ("tail", testTail _, tailReadyToTest),
         ("apply", testApply _, applyReadyToTest),
         ("updated", testUpdated _, updatedReadyToTest),
         ("fastSize", testFastSize _, fastSizeReadyToTest),
         ("fill", testFill _, fillReadyToTest)
    )

  val allInputs = List(
    List(7),
    List(9, 4),
    List(6, 3, 4),
    List(2, 8, 1, 6),
    List(3, 1, 9, 6, 2),
    List(1, 0, 5, 4, 2, 7),
    List(8, 6, 7, 5, 3, 0, 9),
    List(9, 6, 7, 1, 3, 2, 8, 4),
    List(4, 1, 8, 7, 2, 3, 6, 9, 5),
    List(7, 8, 9, 1, 0, 2, 5, 4, 3, 6),
    List(12, 0, 47, 19, 23, 21, 4, 42, 31, 5, 2, 34, 56, 17)
  )

  def testCons(list: List[Int]): Option[String] = {
    val tree = fromList(list)
    if (!checkShapeInvariant(tree)) Some(
      s"  Failed test on $list:\n" +
       "  Resulting tree did not satisfy the shape invariant.\n" +
       "  Tree = \n" +
          tree.draw())
    else None
  }

  def testTail(list: List[Int]): Option[String] = {
    assert(list.nonEmpty)
    val tree = fromList(list)
    val treeOfTail = fromList(list.tail)
    val tailOfTree = tree.tail
    if (!checkShapeInvariant(tailOfTree)) Some(
      s"  Failed test on $list:\n" +
       "  Returned tree did not satisfy the shape invariant.\n" +
       "  Initial tree:\n" +
          tree.draw() +
       "  Returned tree:\n" +
          tailOfTree.draw())
    else if (tailOfTree != treeOfTail) Some(
      s"  Failed test on $list:" +
       "  Initial tree:\n" +
          tree.draw() +
       "  Returned tree:\n" +
          tailOfTree.draw() +
       "  Expected result:\n" +
          treeOfTail.draw())
    else None
  }

  def testApply(list: List[Int]): Option[String] = {
    val tree = fromList(list)
    for (i <- list.indices) {
      val expected = list(i)
      val actual = tree(i)
      if (expected != actual) return Some(
        s"  Failed test on $list:\n" +
         "  Tree:\n" +
            tree.draw() +
        s"  Index = $i\n" +
        s"  Returned result: $actual\n" +
        s"  Expected result: $expected\n")
    }
    None
  }

  def testUpdated(list: List[Int]): Option[String] = {
    val tree = fromList(list)
    for (i <- list.indices) {
      val actual = tree.updated(i, 999)
      val expected = fromList(list.updated(i, 999))
      if (!checkShapeInvariant(actual)) return Some(
        s"  Failed test on $list:\n" +
         "  Returned tree did not satisfy the shape invariant.\n" +
         "  Initial tree:\n" +
            tree.draw() +
        s"  Index = $i\n" +
         "  New value = 999\n" +
         "  Returned tree:\n" +
            actual.draw())
      else if (actual != expected) return Some(
        s"  Failed test on $list:" +
         "  Initial tree:\n" +
            tree.draw() +
         "  Returned tree:\n" +
            actual.draw() +
         "  Expected result:\n" +
            expected.draw())
    }
    None
  }

  def testFastSize(list: List[Int]): Option[String] = {
    val tree = fromList(list)
    val sizeOfList = list.size
    val sizeOfTree = tree.fastSize
    if (sizeOfTree != sizeOfList) Some(
      s"  Failed test on $list:\n" +
       "  Tree = \n" +
          tree.draw() +
      s"  Returned size: $sizeOfTree\n" +
      s"  Expected size: $sizeOfList\n")
    else None
  }

  def testFill(list: List[Int]): Option[String] = {
    // it's kind of silly for this to take a list, but
    // it makes it easier to test all the methods with the same code
    val tree = FPTree.fill(list.size)(list.head)
    if (!checkShapeInvariant(tree)) Some(
      s"  Failed test on size ${list.size} and item ${list.head}\n" +
       "  Returned tree did not satisfy the shape invariant.\n" +
       "  Returned tree:\n" +
          tree.draw())
    else if (tree.size != list.size) Some(
      s"  Failed test on size ${list.size} and item ${list.head}\n" +
       "  Returned tree is the wrong size!\n" +
       "  Returned tree:\n" +
          tree.draw())
    else if (toList(tree).exists(_ != list.head)) Some(
      s"  Failed test on size ${list.size} and item ${list.head}\n" +
      s"  Returned tree contains an element different from ${list.head}.\n" +
       "  Returned tree:\n" +
          tree.draw())
    else None
  }

  def fromList(list: List[Int]): FPTree = {
    list.foldRight(FPTree.empty)(_ +: _)
  }

  def toList(tree: FPTree): List[Int] = {
    if (tree.isEmpty) Nil
    else tree.head :: toList(tree.tail)
  }

  