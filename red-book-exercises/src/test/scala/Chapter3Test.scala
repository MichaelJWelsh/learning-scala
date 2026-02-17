import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import Chapter3.List
import Chapter3.Tree

class Chapter3Test extends AnyFlatSpec with Matchers {
  "Exercise 3.1" should "be 3" in {
    List.x mustBe 3
  }

  "Function 'tail'" should "return the tail of a list" in {
    List.tail(List(1,2,3,4,5)) mustBe List(2,3,4,5)
  }

  "Function 'setHead'" should "set the head to the newVal in a list" in {
    List.setHead(List(1,2,3,4,5), 6) mustBe List(6,2,3,4,5)
  }

  "Function 'drop'" should "drop first n elements in list" in {
    List.drop(List(1,2,3,4,5), 3) mustBe List(4,5)
  }

  "Function 'dropWhile'" should "drop first n elements in list" in {
    List.dropWhile(List(1,3,5,4,6), (a) => a % 2 != 0) mustBe List(4, 6)
  }

  "Function 'init'" should "remove the last element" in {
    List.init(List(1,2,3,4,5)) mustBe List(1,2,3,4)
  }

  "Exercise 3.8" should "be List(1,2,3)" in {
    List.y mustBe List(1,2,3)
  }

  "Function 'length'" should "calculate the length using foldRight" in {
    List.length(List(1,2,3,4,5)) mustBe 5
  }

  "Exercise 3.11" should "have sum/product/length working using foldLeft" in {
    List.sumViaFoldLeft(List(1,2,3,4,5)) mustBe 15
    List.productViaFoldRight(List(1,2,3,4,5)) mustBe 120
    List.lengthViaFoldLeft(List(1,2,3,4,5)) mustBe 5
  }

  "Function 'reverse'" should "reverse the order of elements" in {
    List.reverse(List(1,2,3,4,5)) mustBe List(5,4,3,2,1)
  }

  "Function 'foldRightViaFoldLeft'" should "behave like normal foldRight" in {
    List.foldRightViaFoldLeft(List(1,2,3,4,5), 0, (a, b) => a - b) mustBe List.foldRight(List(1,2,3,4,5), 0, (a, b) => a - b)
  }

  "Function 'foldLeftViaFoldRight'" should "behave like normal foldLeft" in {
    List.foldLeftViaFoldRight(List(1,2,3,4,5), 0, (b, a) => a - b) mustBe List.foldLeft(List(1,2,3,4,5), 0, (b, a) => a - b)
  }

  "Function 'appendViaFoldLeft'" should "behave like normal append" in {
    List.appendViaFoldLeft(List(1,2,3,4,5), List(6,7,8)) mustBe List.append(List(1,2,3,4,5), List(6,7,8))
  }

  "Function 'flatten'" should "turn a list of lists into a single list" in {
    List.flatten(List(List(1,2,3,4,5), List(6,7,8))) mustBe List(1,2,3,4,5,6,7,8)
  }

  "Function 'plusOne'" should "add 1 to every number" in {
    List.plusOne(List(1,2,3,4,5)) mustBe List(2,3,4,5,6)
  }

  "Function 'listOfDoubleToString'" should "convert every double to a string" in {
    List.listOfDoubleToString(List(1,2,3,4,5)) mustBe List("1.0", "2.0", "3.0", "4.0", "5.0")
  }

  "Function 'map'" should "apply function to every element" in {
    List.map(List(1, 2, 3, 4, 5), _ * 2) mustBe List(2,4,6,8,10)
  }

  "Function 'filter'" should "remove every element that doesn't meet the criteria" in {
    List.filter(List(1, 2, 3, 4, 5), _ % 2 == 0) mustBe List(2, 4)
  }

  "Function 'flatMap'" should "apply function to every element then flatten" in {
    List.flatMap(List(1, 2, 3, 4, 5), a => List(a * 2)) mustBe List(2, 4, 6, 8, 10)
  }

  "Function 'filterViaFlatMap'" should "behave like regular filter" in {
    List.filterViaFlatMap(List(1, 2, 3, 4, 5), _ % 2 == 0) mustBe List.filter(List(1, 2, 3, 4, 5), _ % 2 == 0)
  }

  "Function 'addLists'" should "pairwise add ints together using both lists" in {
    List.addLists(List(1,2,3), List(4,5,6)) mustBe List(5,7,9)
  }

  "Function 'addListsGeneric'" should "pairwise add both lists together using provided function" in {
    List.addListsGeneric(List(1,2,3), List(4,5,6), (a,b) => a + 2 * b) mustBe List(9,12,15)
  }

  "Function 'hasSubsequence'" should "determine if the subsequence is in the given sequence" in {
    List.hasSubsequence(List(1,2,3,4,5), List(3,4)) mustBe true
    List.hasSubsequence(List(1,2,3,4,5), List(4,5,6)) mustBe false
  }

  val t: Tree[Int] = Tree.Branch(Tree.Branch(Tree.Branch(Tree.Leaf(-5), Tree.Leaf(-3)), Tree.Leaf(0)), Tree.Branch(Tree.Leaf(5), Tree.Leaf(10)))

  "Function 'maximum'" should "return the max element" in {
     t.maximum mustBe 10
  }

  "Function 'depth'" should "return the max depth" in {
    Tree.depth(t) mustBe 3
  }

  val t2: Tree[Double] = Tree.Branch(Tree.Branch(Tree.Branch(Tree.Leaf(-12.5), Tree.Leaf(-7.5)), Tree.Leaf(0)), Tree.Branch(Tree.Leaf(12.5), Tree.Leaf(25)))
  "Function 'map'" should "apply mapping function to each leaf in tree" in {
    Tree.map(t, _ * 2.5) mustBe t2
  }

  "Exercise 3.28" should "have size/maximum/depth/map working using fold" in {
    Tree.sizeFromFold(t) mustBe Tree.size(t)
    Tree.maximumFromFold(t) mustBe Tree.maximum(t)
    Tree.depthFromFold(t) mustBe Tree.depth(t)
    Tree.mapFromFold(t, _ * 2.3) mustBe Tree.map(t, _ * 2.3)
  }
}