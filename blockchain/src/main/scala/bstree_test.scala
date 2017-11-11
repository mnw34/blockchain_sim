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

    var l = Array[Int]()
    println("\ninorder " + root.inorder(l).mkString(" "))
    l = Array[Int]()
    println("preorder " + root.preorder(l).mkString(" "))
    l = Array[Int]()
    println("postorder " + root.postorder(l).mkString(" "))

  }
}
