import crypto.hashable

object merkelTree_test {

  class hashableInt(var v : Int) extends hashable {
    def getData = v.toHexString.getBytes
    def setData(data : Array[Byte]) = v = Integer.parseInt(data.toString,16)
    override def toString = v.toString
  }

  def main(args : Array[String]) {
    val numRand    = args(0).toInt
    val randRange  = args(1).toInt
    var rand = scala.util.Random

    // print function
    val printInt = (x : bst[hashable]) => {
      var s = x.asInstanceOf[merkelTree].value.toString
      if (1 == s.split(" ").length)
        print("leaf <" + s + "> ")
      else if (0 != s.split(" ").length)
        println("\n" + s)
    }

    implicit def bst2merkelTree(n : bst[hashable]) : merkelTree = {
      n.asInstanceOf[merkelTree]
    }

    def isValidMerkelTree(x : merkelTree) : Boolean = {

      // check parent association
      var isParentValid = (null == x.parent) ||
                          (x == x.parent.left ^ x == x.parent.right)

      var isLeaf = (null == x.left && null == x.right)

      var isChildrenValid = isLeaf ||
        (x == x.left.parent && isValidMerkelTree(x.left) &&
         x == x.right.parent && isValidMerkelTree(x.right))

      var isStructureValid = isLeaf || x.left.height >= x.right.height

      var isValueValid = true
      if (!isLeaf) {
        var nodeCmp = new merkelTree(new merkelTreeHash(x.left.value, x.right.value))
        isValueValid = x.value.getData.sameElements(nodeCmp.getData)
      }

      isParentValid && isChildrenValid && isStructureValid && isValueValid
    }

    // create root node
    var r = new hashableInt(rand.nextInt(randRange))
    println("inserting node " + r)
    var root = new merkelTree(r)

    var printAll = (r : merkelTree) => {
      println("\nheight " + r.height)
      //print("inorder ")
      //r.inorder(printInt)
      //print("\npreorder ")
      //r.preorder(printInt)
      print("\npostorder ")
      r.postorder(printInt)
      println
    }

    // insert a bunch of random nodes
    for (i <- 0 to numRand) {
      r = new hashableInt(rand.nextInt(randRange))
      println("inserting node " + r)
      root = root.insert(new merkelTree(r))
      printAll(root)
      if (!isValidMerkelTree(root)) throw new Exception("Invalid Merkel tree")
    }

    println("\nTrying to find random values...")
    for (i <- 0 to numRand) {
      r = new hashableInt(rand.nextInt(randRange))
      var x = root.find(r)
      print("root.find(" + r + ") : ")
      while (x != null) {
        print(" " + x.value)
        x = x.parent
      }
      println()
    }

    printAll(root)
  }
}
