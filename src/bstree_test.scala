object bstree_test {

  def main(args : Array[String]) {
    val numRand    = args(0).toInt
    val randRange  = args(1).toInt
    var rand = scala.util.Random

    // compare function
    val lessThan = (x : Int, y : Int) => x < y

    // create root node
    var root = new bstree(rand.nextInt(randRange), lessThan)

    // insert a bunch of random nodes
    for (i <- 0 to numRand) {
      var t = new bstree[Int](rand.nextInt(randRange), lessThan)
      root.insert(t)
    }

    var l = Array[Int]()
    println(root.inorder(l).mkString(" "))
    l = Array[Int]()
    println(root.preorder(l).mkString(" "))
    l = Array[Int]()
    println(root.postorder(l).mkString(" "))

  }
}
