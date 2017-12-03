object BST_test {

  def main(args : Array[String]) {
    val numRand    = args(0).toInt
    val randRange  = args(1).toInt
    var rand = scala.util.Random

    // compare functions
    val lessThan = (x : Int, y : Int) => x < y
    val equals = (x : Int, y : Int) => x == y

    // print function
    val printInt = (x : BST[Int]) => {
      print(x.value + " ")
    }

    def isValidBST(x : BST[Int]) : Boolean = {

      // check parent association
      var isParentValid = (null == x.parent) ||
                          (x == x.parent.left ^ x == x.parent.right)

      var isLeftValid = (null == x.left) ||
        (x == x.left.parent &&
          ((equals(x.value, x.left.value) || lessThan(x.left.value, x.value))
            && isValidBST(x.left)))

      var isRightValid = (null == x.right) ||
        (x == x.right.parent &&
          ((equals(x.value, x.right.value) || lessThan(x.value, x.right.value))
            && isValidBST(x.right)))

      isParentValid && isLeftValid && isRightValid
    }

    // create root node
    var r = rand.nextInt(randRange)
    print("adding nodes: " + r)
    var root = new BST(r, lessThan, equals)

    var printAll = (r : BST[Int]) => {
      println("\nheight " + r.height)
      print("inorder ")
      r.inorder(printInt)
      print("\npreorder ")
      r.preorder(printInt)
      print("\npostorder ")
      r.postorder(printInt)
      println
    }

    // insert a bunch of random nodes
    for (i <- 0 to numRand) {
      r = rand.nextInt(randRange)
      print(" " + r)
      var t = new BST[Int](r, lessThan, equals)
      root.insert(t)
      printAll(root)
      if (!isValidBST(root)) throw new Exception("Invalid BST ")
    }

    println("\nTrying to find random values...")
    for (i <- 0 to numRand) {
      r = rand.nextInt(randRange)
      var x = root.find(r)
      var t = if (null == x) -1 else x.value
      print("root.find(" + r + ") : ")
      while (x != null) {
        print(" " + x.value)
        x = x.parent
      }
      println()
    }

    printAll(root)

    println("\nDoing some random deletions and rerunning traversals")
    for (i <- 0 to numRand << 1) {
      r = rand.nextInt(randRange)
      if (null != root.find(r)) {
        println("Deleting " + r);
        root = root.delete(r)

        if (root != null) {
          printAll(root)
          if (!isValidBST(root)) throw new Exception("Invalid BST ")
        }
        else {
          println("empty tree")
        }
      }
    }

    return 0
  }
}
