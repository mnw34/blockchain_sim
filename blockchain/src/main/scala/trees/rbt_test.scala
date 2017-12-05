import scala.collection.mutable.ListBuffer

object rbt_test {

  // extend the rbtree to get the node color during traversals
  class rbtColor[T](t : T, lessThan : (T, T) => Boolean, equals : (T, T) => Boolean)
    extends rbt(t, lessThan, equals) {

    // override the traversal operators to print the color
    override def inorder(f : bst[T] => Unit) : Unit = {
      if (null != left) left.inorder(f)
      f(this)
      print(color.toString + " ")
      if (null != right) right.inorder(f)
    }
    override def preorder(f : bst[T] => Unit) : Unit = {
      f(this)
      print(color.toString + " ")
      if (null != left) left.preorder(f)
      if (null != right) right.preorder(f)
    }
    override def postorder(f : bst[T] => Unit) : Unit = {
      if (null != left) left.postorder(f)
      if (null != right) right.postorder(f)
      f(this)
      print(color.toString + " ")
    }
  }

  def main(args : Array[String]) {
    var numRand = 0
    var randRange = 0
    var rand = scala.util.Random

    // cast
    def asRb[T](x: bst[T]): rbt[T] = x.asInstanceOf[rbt[T]]

    // compare functions
    val lessThan = (x : Int, y : Int) => x < y
    val equals = (x : Int, y : Int) => x == y

    var numNodes = 0
    // print function
    def printInt(x : bst[Int]) = {
      print(x.value + " ")
      numNodes += 1
    }

    var printAll = (r : bst[Int]) => {
      if (null == r) {
        println("Empty tree.")
        return
      }
      println("\nheight " + r.height)
      print("inorder  ")
      numNodes = 0
      r.inorder(printInt)
      println("numNodes " + numNodes)
      print("preorder ")
      numNodes = 0
      r.preorder(printInt)
      println("numNodes " + numNodes)
      print("postorder ")
      numNodes = 0
      r.postorder(printInt)
      println("numNodes " + numNodes)
    }

    def blackHeight(x : rbt[Int]) : Int = {
      val invalid : Int = 0xffffffff
      if (null == x)
        1
      else if (DBLACK == x.color)
        invalid
      else {
        var lH = bH(x.left)
        if (lH != bH(x.right))
          invalid
        else if (BLACK == x.color)
          lH + 1
        else
          lH
      }
    }

    def bH(x : bst[Int]) : Int = {
      if (null == x) {
        1 // black null leaf
      } else {
        blackHeight(asRb(x))
      }
    }

    def isValidRBT(x : rbt[Int]) : Boolean = {

      // check parent association
      var a = (null == x.parent)
      var b = true
      var isParentValid = a || (x == x.parent.left ^ x == x.parent.right) /*(null == x.parent) ||
                          (x == x.parent.left ^ x == x.parent.right) */

      a = (null == x.left)
      b = a || (x == x.left.parent)
      var c = a || b && (equals(x.value, x.left.value) || lessThan(x.left.value, x.value))
      var d = a || c && isValidRBT(asRb(x.left))

      var isLeftValid = a || b && c && d /* (null == x.left) ||
        (x == x.left.parent &&
          ((equals(x.value, x.left.value) || lessThan(x.left.value, x.value))
            && isValidRBT(asRb(x.left)))) */

      a = (null == x.right)
      b = a || x == x.right.parent
      c = a || b && (equals(x.value, x.right.value) || lessThan(x.value, x.right.value))
      d = a || c && isValidRBT(asRb(x.right))

      var isRightValid = a || b && c && d /* (null == x.right) ||
        (x == x.right.parent &&
          ((equals(x.value, x.right.value) || lessThan(x.value, x.right.value))
            && isValidRBT(asRb(x.right)))) */


      a = (null == x.parent && BLACK == x.color)
      b = a || (DBLACK != x.color)
      c = a || b && !(RED == x.color && RED == asRb(x.parent).color)

      var isValidColor = a || b && c /* (null == x.parent && BLACK == x.color) ||
        ((DBLACK != x.color) && !(RED == x.color && RED == asRb(x.parent).color)) */

      var rH = bH(x.right)
      var lH = bH(x.left)

      var isValidBlackHeight = rH == lH

      return isValidBlackHeight && isValidColor && isParentValid && isLeftValid && isRightValid
    }

    if (2 != args.length) {

      var rbt = new rbtColor(7, lessThan, equals)

      // insert
      var l = ListBuffer(1, 7, 8, 8)
      for (i <- 0 until l.length) {
        println("Inserting " + l(i))
        var t = new rbtColor[Int](l(i), lessThan, equals)
        rbt = rbt.insert(t).asInstanceOf[rbtColor[Int]]
        if (!isValidRBT(rbt)) {
          throw new Exception("Invalid RBT")
        }
        printAll(rbt)
      }

      // delete
      var k = ListBuffer(1, 8)
      for (i <- 0 until k.length) {
        println("Deleting " + k(i))
        var t = rbt.delete(k(i))
        if (null != t)
          rbt = t.asInstanceOf[rbtColor[Int]]
        else
          rbt = null
        printAll(rbt)
        if (!isValidRBT(rbt)) {
          throw new Exception("Invalid RBT")
        }
      }
    }
    else {

      numRand = args(0).toInt
      randRange = args(1).toInt

      var r = rand.nextInt(randRange)
      println("adding node: " + r)
      var rbt = new rbtColor(r, lessThan, equals)
      var z = 1

      // insert a bunch of random nodes
      var l = ListBuffer(r)
      for (i <- 1 until numRand) {
        r = rand.nextInt(randRange)
        print("adding node: " + r + " ")
        var t = new rbtColor[Int](r, lessThan, equals)
        rbt = rbt.insert(t).asInstanceOf[rbtColor[Int]]
        l += r
        printAll(rbt)
        if (!isValidRBT(rbt))
          throw new Exception("Invalid RBT ")
        else if (null != rbt.parent)
          throw new Exception("Invalid RBT reference not root")
        else if (numNodes != z + 1)
          throw new Exception("Invalid number of nodes after insert"+ numNodes)
        else
          z = numNodes
      }

      println("\nTrying to find random values...")
      for (i <- 2 to numRand) {
        r = rand.nextInt(randRange)
        var x = rbt.find(r)
        var t = if (null == x) -1 else x.value
        print("rbt.find(" + r + ") : ")
        while (x != null) {
          print(" " + x.value)
          x = asRb(x.parent)
        }
        println
      }


      print("Inserted ")
      for (i <- l)
        print(i + " ")

      printAll(rbt)

      println("\nDoing some random deletions and rerunning traversals")
      var i = 0
      while (null != rbt) { //i != 10) {
        i += 1
        r = rand.nextInt(randRange)
        if (null != rbt.find(r)) {
          println("Deleting " + r)
          var t = rbt.delete(r)
          if (t != null) {
            rbt = t.asInstanceOf[rbtColor[Int]]
            printAll(rbt)
            if (!isValidRBT(rbt))
              throw new Exception("Invalid RBT ")
            else if (null != rbt.parent)
              throw new Exception("Invalid RBT reference not root")
            else if (z - 1 != numNodes)
              throw new Exception("Invalid num nodes after delete " + numNodes)
            else
              z = numNodes
          }
          else {
            rbt = null
            println("empty tree")
          }
        }
      }
    }
  }
}
