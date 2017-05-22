
object Solution {
  def main(args: Array[String]) = ValidBST.main(args)
}


object ValidBST {
  val sc = new java.util.Scanner (System.in);

  def readTraversal(): List[Int] = {
    val n = sc.nextInt()
    var arr = new Array[Int](n)
    for(i <- 0 to n-1)
      arr(i) = sc.nextInt()

    arr.toList
  }

  // https://www.hackerrank.com/challenges/valid-bst/forum/comments/101479

  trait Tree
  case class Node(x: Int, left: Tree, right: Tree) extends Tree
  case object Empty extends Tree

  def constructNode(head: Int, arr: List[Int]): Tree = {
    val leftArr = arr.filter(_<head)
    val rightArr = arr.filter(_>head)

    val leftTree = leftArr match {
      case List() => Empty
      case x::xs => constructNode(x,xs)
    }
    val rightTree = rightArr match {
      case List() => Empty
      case y::ys => constructNode(y,ys)
    }

    Node(head, leftTree, rightTree)
  }

  def constructTree(arr: List[Int]): Tree = arr match {
    case x::xs => constructNode(x, xs)
    case List() => Empty
  }

  // https://en.wikipedia.org/wiki/Tree_traversal#Post-order
  def postOrderTraversal(t: Tree): List[Int] = t match {
    case Empty => List()
      //case Node(x, leftT, rightT) => postOrderTraversal(leftT):::postOrderTraversal(rightT):::List(x)
    case Node(x, leftT, rightT) =>
      x::postOrderTraversal(leftT):::postOrderTraversal(rightT)
  }


  def main(args: Array[String]) {
    val n = sc.nextInt()
    for(i <- 1 to n) {
      val arr = readTraversal()
      val t = constructTree(arr)
      val arr2 = postOrderTraversal(t)
      if(arr.sameElements(arr2)){
        println("YES")
      } else {
        println("NO")
      }
    }

  }
}
