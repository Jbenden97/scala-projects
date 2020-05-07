object hw9 extends App {
  val userName = "Joey Benden"

  //////////////////////////////////////////////////
  // Problem 1: Goody Bags

  // DON'T FORGET TO INCLUDE BIG-O!
  // O(n^2 + n)
  def numGoodyBags(k: Int, numByKind: Array[Int]): Int = {
    // k is the number of goodys that go in each bag
    // numByKind contains the number of items of each different kind
    //   for example, (4,14,7) might mean that you have
    //   4 pencils, 14 lollipops, and 7 whistles
    val itemsQueue = scala.collection.mutable.PriorityQueue.empty[Int]
      numByKind.foreach {i =>
      itemsQueue.enqueue(i)
    }

    var bagCount: Int = 0

    while (itemsQueue.length >= k) {
      val topItems = (1 to k).map { i =>
        itemsQueue.dequeue
      }
      bagCount += 1;
      topItems.foreach { i =>
        if (i > 1)
          itemsQueue.enqueue(i - 1)
      }
    }
    bagCount
  }
  // change the following flag to true when you're ready to test Problem 1
  val numGoodyBagsReadyToTest = true

  //////////////////////////////////////////////////
  // Problem 2: Plausible History

  // Study the following trait, case object, and case class,
  // BUT DO NOT CHANGE THEM!
  sealed trait LHeap {
    def size: Int
    def insert(item: Char): LHeap
  }
  case object Empty extends LHeap {
    def size: Int = 0
    def insert(item: Char): LHeap = Node(item, Empty, Empty)
  }
  case class Node(item: Char, left: LHeap, right: LHeap) extends LHeap {
    val size = left.size + right.size + 1
    def insert(item: Char): LHeap = {
      if (item < this.item) Node(item, this, Empty)
      else mkNode(this.item, left, right.insert(item))
    }
    private def mkNode(item: Char, left: LHeap, right: LHeap): LHeap = {
      if (left.size >= right.size) Node(item, left, right)
      else Node(item, right, left)
    }
  }

  // and this is the function that you'll write
  // DON'T FORGET TO INCLUDE BIG-O!
  //O(logn)
  def history(heap: LHeap): List[Char] = heap match {
      case Empty => List()
      case Node(i, l, r) =>
        history(l) ++ List(i) ++ history(r)
    }


  // change the following flag to true when you're ready to test Problem 2
  val historyReadyToTest = true

  //////////////////////////////////////////////////
  // EVERYTHING BELOW THIS LINE IS RELATED TO TESTING
  // DON'T CHANGE ANY OF IT

  println(userName)

  import scala.util.control.Breaks._

  if (numGoodyBagsReadyToTest) {
    println(">>> Begin testing numGoodyBags")
    var passed = 0
    var failed = 0
    breakable {
      for ((k, numByKind, expectedAnswer) <- testData) {
        val answer = numGoodyBags(k, numByKind)
        if (answer == expectedAnswer) passed += 1
        else {
          println("Failed test:")
          println(s"  k = $k")
          println(s"  numByKind = ${numByKind.mkString("(", ",", ")")}")
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
    println(s"Passed $passed tests.")
  } else {
    println("*** Skipping tests for numGoodyBags")
  }

  if (historyReadyToTest) {
    def randLetters(len: Int): List[Char] = {
      val letters = ('a' to 'z').toArray
      for (i <- 0 until len) {
        val j = i + (math.random * (26 - i)).toInt
        val tmp = letters(i)
        letters(i) = letters(j)
        letters(j) = tmp
      }
      letters.take(len).toList
    }
    def makeTree(chars: List[Char]): LHeap = {
      var tree: LHeap = Empty
      for (c <- chars) tree = tree.insert(c)
      tree
    }

    println(">>> Begin testing history") 
    var passed = 0
    breakable {
      for (len <- 0 to 20; reps <- 1 to 3 * len + 1) {
        val originalInput = randLetters(len)
        val tree1 = makeTree(originalInput)
        val answer = history(tree1)
        val tree2 = makeTree(answer)
        if (tree1 == tree2) passed += 1
        else {
          println("Failed test:")
          println("  Input tree =")
          draw(tree1)
          println(s"  Your output: $answer")
          println("  But that would create this tree instead:")
          draw(tree2)
          println("Aborting remaining tests.")
          break
        }
      }
    }
    println(s"Passed $passed tests.")
  } else {
    println("*** Skipping tests for history")
  }

  //////////////////////////////////////////////////
  // tree printing utility
  import scala.collection.mutable.ArrayBuffer
  case class Box(
      width: Int,
      rootLeft: Int,
      rootRight: Int,
      lines: ArrayBuffer[String]
  ) {
    def height: Int = lines.length
    def padTo(desiredHeight: Int): Unit = {
      while (height < desiredHeight) {
        lines += " " * width
      }
    }
  }

  def spaces(n: Int): String = " " * n

  def makeBox(tree: LHeap): Box = tree match {
    case Empty => Box(1, 0, 0, ArrayBuffer("*"))
    case Node(item, left, right) =>
      val leftBox = makeBox(left)
      val rightBox = makeBox(right)
      leftBox.padTo(rightBox.height)
      rightBox.padTo(leftBox.height)

      val minRootWidth = (leftBox.width - leftBox.rootRight - 2) + 1 + (rightBox.rootLeft - 1)
      var root = item.toString
      while (root.length < minRootWidth) root = "_" + root + "_"
      root = spaces(leftBox.rootRight + 2) + root + spaces(
        rightBox.width - rightBox.rootLeft + 1
      )

      val centerPadding = spaces(root.length - leftBox.width - rightBox.width)
      val totalWidth = leftBox.width + centerPadding.length + rightBox.width
      for (i <- leftBox.lines.indices) {
        leftBox.lines(i) = leftBox.lines(i) + centerPadding + rightBox.lines(i)
      }
      val middleWidth = totalWidth - leftBox.rootRight - (rightBox.width - rightBox.rootLeft) - 3
      val connections =
        spaces(leftBox.rootRight + 1) + "/" +
          spaces(middleWidth) +
          "\\" + spaces(rightBox.width - rightBox.rootLeft)

      Box(
        totalWidth,
        root.indexWhere(_.isLetter),
        root.lastIndexWhere(_.isLetter),
        root +: connections +: leftBox.lines
      )
  }

  def draw(tree: LHeap, leftPadding: Int = 4): Unit = {
    // draws a picture of the tree, GREAT FOR DEBUGGING!
    // print the resulting string to see the picture
    val box = makeBox(tree)
    for (line <- box.lines)
      println(spaces(leftPadding) + line)
  }

//////////////////////////////////////////////////
  def testData =
    """
2 4 14 7 11
4 5 5 5 5 5 6
7 99 99 99 99 99 99 0
10 1 2 3 4 5 0
2 96 62 42 100
8 27 94 76 69 2 10 27 25 2 37 83 93 30 60 77 87
5 63 48 65 92 41 53 55 81
3 21 2 5 91 7
2 46 42 58 42 33 58 63 25 52 23 221
3 8 27 64 87 23 96 101
4 14 31 74 39 87 42
3 80 70 1 17 69 23 64 56 35 138
5 36 20 38 89 60 87 47
6 48 78 32 40 27 76 74 62 83 57 76 56 84 85 16 49 79 64 75 70 205
10 29 59 52 85 85 24 91 48 48 22 68 50 14 87 31 52 90 56 62 105
6 30 55 36 84 71 96 94 72 49 28 53 8 26 117
10 43 45 38 41 69 84 10 32 76 22 84 9 36
2 12 34 39 26 66 88 132
4 83 19 25 67 9 17 38 38 21 58 11 91 1 36 70 91 41 60 194
10 35 46 44 32 19 94 69 81 62 40 97 32 99 42 70
2 17 13 13
4 90 20 80 21 58 15 23 82 69 36 123
3 49 93 48 96 43 43 24 81 159
8 85 66 72 26 81 51 65 16 61 69 25 29 15 82
9 58 54 27 27 96 64 60 63 79 52 53
7 14 54 50 24 11 61 10 42 38 96 81 7 56 6 1 74
10 43 75 87 63 55 24 80 26 81 7 56 24 53 50 78 61 86
5 78 78 55 49 34 11 45
7 73 73 95 56 93 23 85 21 87 73 23 100
4 79 13 99 52 56 60
10 48 69 94 13 33 21 23 50 55 35 22 88 33 99 74 36 55 13 98 74 103
2 21 51 21
5 70 17 39 64 64 60 8 89 39 90
4 18 61 69 17 77 6 95 82
10 73 95 15 27 52 55 64 58 73 26 73 90 53 75 28 59 6 27 94
6 72 23 84 17 71 83 17
5 83 60 70 45 91 57 81 26 24 57 76 71 74 6 63 30 182
10 19 72 73 4 97 77 17 74 71 11 26 48 21 30 17 38 57
10 5 51 26 97 26 35 65 23 28 37 53 87 19 59 4 73 98 17 49 36 86
10 72 34 74 85 46 41 58 73 24 94 95 79 3 24 51 4 83 58 99
3 2 85 22 2
10 13 82 18 85 32 92 41 76 92 71 85 91 80 99 58 26 90 17 8 115
3 45 70 27 27
5 41 65 17 71 76 65 94 83
3 32 42 90 14 71 79
2 47 83 7 54
8 22 72 64 78 53 67 6 94 6
2 3 48 6 44 40 97 35 68 38 65 66 38 30 66 49 7 350
5 4 73 54 16 6 14 95 47 72 35 7 59 66 109
10 89 27 34 39 73 62 50 79 43 82 5 44 6 37 99 61 40 91 21 58 104
2 33 52 55 46 79 46 84 81 64 48 4 31 7 95 43 384
10 79 2 85 70 62 45 53 26 44 40 59 93 5 11 78 69
2 8 56 8
2 51 69 42 43 61 4 18 144
10 2 14 2 7 5 66 43 99 88 95 44 80 44 15
10 49 17 81 33 2 62 61 6 62 90 1 5 55 24 56 37 38 63
10 72 40 31 13 34 2 13 65 82 57 73 86 39 57 87 69 30 84
2 68 22 16 81 61 12 31 96 81 30 88 293
3 57 96 89 30 61 46 4 40 141
10 70 10 37 90 93 87 41 67 40 10 36 18 51 67 60
3 14 69 31 32 90 44 53 111
2 70 38 53 5 97 131
3 67 72 52 52
7 92 15 32 76 20 59 6 97 69 36
8 36 95 24 94 50 69 9 33 5 81 98 69 7 41 95 89 54 53 16 8 128
2 11 24 68 28 41 9 53 41 30 23 4 36 184
3 49 3 70 3
10 88 60 74 51 70 51 46 55 60 85 94 14 67
2 53 13 9 89 25 53 11 81 32 70 218
10 85 63 73 48 61 85 4 11 36 62 87 39 9 19 13 71 39 4 68 18 89
6 48 23 27 27 34 88 28 34
10 93 71 33 77 20 35 5 64 18 65 6 19 90 33
8 67 67 54 21 7 83 46 48 47 40 91 19 71 65 76 57 107
2 93 10 40 40 90
2 88 61 20 64 116
9 7 19 51 28 12 94 46 19 98 22 40 22 58 28 48
2 35 6 37 39
10 33 14 93 55 8 65 99 14 48 83 86 64 8 31 52
5 26 65 41 23 91 44 49 6 82 94 85 121
9 62 78 68 31 99 60 66 65 59 52 66 25 76 20 12 32 96
2 10 49 21 31
10 86 31 55 57 28 98 95 32 3 49 78 29 42 87 14 1 53 46 10 87
10 28 24 20 98 28 64 72 21 53 10 51 18 24 98 43
3 2 63 65 5 7
5 38 67 27 39 29 86 77 71 60 67 47 39 91 48 24 70 40 30 190
5 37 62 21 59 6 6
2 99 62 56 108
4 34 46 21 76 8 76 60 4 84 81 49 55 30 22 84 36 33 40 50 1 222
3 40 62 75 22 62
2 61 42 42
10 75 52 47 96 86 99 69 29 89 18 51 15 16 57
7 56 93 42 30 39 91 43 96 29 80 48 64 59 110
10 68 8 26 20 98 90 29 85 79 33 23 68 98 38 19 32 98 49 34 99
2 54 80 94 93 44 26 5 10 60 42 64 286
2 66 94 66
10 72 35 30 2 12 91 18 73 28 62 45 24 28
2 13 45 9 18 40
7 19 17 75 87 67 56 44 18 31 10 84 65
5 56 6 82 85 19 6
10 13 23 51 75 36 45 43 71 64 44 42 15 47 81 63 17 98 35 93 95
""".trim
      .split("\n")
      .map(_.trim.split(" ").map(_.toInt))
      .map(arr => (arr.head, arr.slice(1, arr.length - 1), arr.last))
}
