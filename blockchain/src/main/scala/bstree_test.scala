object bstree_test {

  def main(args : Array[String]) {
    val numRand    = args(0).toInt
    val randRange  = args(1).toInt
    var rand = scala.util.Random

    // compare function
    val lessThan = (x : Int, y : Int) => x < y

    // create root node
    var r = rand.nextInt(randRange)
    print("adding nodes: " + r)
    var root = new bstree(r, lessThan)

    // insert a bunch of random nodes
    for (i <- 0 to numRand) {
      r = rand.nextInt(randRange)
      print(" " + r)
      var t = new bstree[Int](r, lessThan)
      root.insert(t)
    }

    println("\nTrying to find random values...")
    for (i <- 0 to numRand) {
      r = rand.nextInt(randRange)
      var x = root.find(r)
      var t = if (null == x) -1 else x.t
      print("root.find(" + r + ") : ")
      while (x != null) {
        print(" " + x.t)
        x = x.parent
      }
      println()
    }

    var l = Array[Int]()
    println("\ninorder " + root.inorder(l).mkString(" "))
    l = Array[Int]()
    println("preorder " + root.preorder(l).mkString(" "))
    l = Array[Int]()
    println("postorder " + root.postorder(l).mkString(" "))

    println("\nDoing some random deletions and rerunning traversals")
    for (i <- 0 to numRand << 1) {
      r = rand.nextInt(randRange)
      var x = root.find(r)
      if (null != x) {
        println("Deleting " + r);
        if (root == x) root = x.delete()
        else x.delete()
        l = Array[Int]()
        println("\ninorder " + root.inorder(l).mkString(" "))
        l = Array[Int]()
        println("preorder " + root.preorder(l).mkString(" "))
        l = Array[Int]()
        println("postorder " + root.postorder(l).mkString(" "))
      }
    }


  }
}
