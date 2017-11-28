object rbtree_test {

  // extend the rbtree to get the node color during traversals
  class rbtreeColor[T](t : T, lessThan : (T, T) => Boolean, equals : (T, T) => Boolean)
    extends rbtree(t, lessThan, equals) {

    // override the traversal operators to print the color
    override def inorder(f : bstree[T] => Unit) : Unit = {
      if (null != left) left.inorder(f)
      f(this)
      print(color.toString + " ")
      if (null != right) right.inorder(f)
    }
    override def preorder(f : bstree[T] => Unit) : Unit = {
      f(this)
      print(color.toString + " ")
      if (null != left) left.preorder(f)
      if (null != right) right.preorder(f)
    }
    override def postorder(f : bstree[T] => Unit) : Unit = {
      if (null != left) left.postorder(f)
      if (null != right) right.postorder(f)
      f(this)
      print(color.toString + " ")
    }
  }

  def main(args : Array[String]) {
    val numRand    = args(0).toInt
    val randRange  = args(1).toInt
    var rand = scala.util.Random

    // cast
    def asRb[T](x: bstree[T]): rbtree[T] = x.asInstanceOf[rbtree[T]]

    // compare functions
    val lessThan = (x : Int, y : Int) => x < y
    val equals = (x : Int, y : Int) => x == y

    // print function
    val printInt = (x : bstree[Int]) => print(x.value + " ")

    // create rbt node
    var r = rand.nextInt(randRange)
    println("adding nodes: " + r)
    var rbt = new rbtreeColor(r, lessThan, equals)

    // insert a bunch of random nodes
    for (i <- 0 to numRand) {
      r = rand.nextInt(randRange)
      print(r + " ")
      var t = new rbtreeColor[Int](r, lessThan, equals)
      rbt.insert(t)
      // update rbt node in case of rebalancing
      rbt = rbt.root.asInstanceOf[rbtreeColor[Int]]
      print("height " + rbt.height + " preorder ")
      rbt.preorder(printInt)
      println
    }

    println("\nTrying to find random values...")
    for (i <- 0 to numRand) {
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

    var printAll = (r : bstree[Int]) => {
      println("\nheight " + r.height)
      print("inorder  ")
      r.inorder(printInt)
      println
      print("preorder ")
      r.preorder(printInt)
      println
      print("postorder ")
      r.postorder(printInt)
      println
    }


    printAll(rbt)

    println("\nDoing some random deletions and rerunning traversals")
    for (i <- 0 to numRand << 1) {
      r = rand.nextInt(randRange)
      var x = rbt.find(r)
      if (null != x) {
        println("Deleting " + r)
        if (rbt == x)
          rbt = asRb(x.delete).asInstanceOf[rbtreeColor[Int]]
        else x.delete

        if (rbt != null) {
          printAll(rbt)
        }
        else {
          println("empty tree")
        }
      }
    }
  }
}
