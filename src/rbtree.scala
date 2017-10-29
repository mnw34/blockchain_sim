// rbtree.scala
object Color {
  val RED   = 0
  val BLACK = 1
}

class rbtree {
  var color  : int
  val right  : rbtree = null
  val left   : rbtree = null
  val parent : rbtree = null

  def insert(node : rbtree) : Unit = {
    // first step is color inserted node red
    node.color = Color.RED

    if (right == null) { 
      right = node
      node.parent = this 
    }
    else { 
      right.insert(node) 
    }

    // check for red parent
    if (Color.RED == node.parent.color) {

    }
  }

}

} 
