object bstree_test {

  def main(args : Array[String]) {
    val numRand    = args(0).toInt
    val randRange  = args(1).toInt
    var rand = scala.util.Random

    // compare functions
    val lessThan = (x : Int, y : Int) => x < y
    val equals = (x : Int, y : Int) => x == y

    // print function
    val printInt = (x : bstree[Int]) => {
      print(x.value + " ")
    }

    // create root node
    var r = rand.nextInt(randRange)
    print("adding nodes: " + r)
    var root = new bstree(r, lessThan, equals)

    // insert a bunch of random nodes
    for (i <- 0 to numRand) {
      r = rand.nextInt(randRange)
      print(" " + r)
      var t = new bstree[Int](r, lessThan, equals)
      root.insert(t)
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

    var printAll = (r : bstree[Int]) => {
      println("\nheight " + r.height)
      r.inorder(printInt)
      println
      r.preorder(printInt)
      println
      r.postorder(printInt)
      println
    }

    printAll(root)

    println("\nDoing some random deletions and rerunning traversals")
    for (i <- 0 to numRand << 1) {
      r = rand.nextInt(randRange)
      var x = root.find(r)
      if (null != x) {
        println("Deleting " + r);
        if (root == x)
          root = x.delete
        else x.delete

        if (root != null) {
          printAll(root)
        }
        else {
          println("empty tree")
        }
      }
    }
  }
}
