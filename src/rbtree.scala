// rbtree.scala
// red black tree
object Color {
  val RED    = 0
  val BLACK  = 1
  val DBLACK = 2
}

class rbtree[T](override t : T, override lessThan : T, T => Boolean  
  extends bstree(t, lessThan) {

  var color  : Int

  override def insert(node : rbtree) : Unit = {
    // first step is color inserted node red
    node.color = Color.RED

    // call the binary search tree insert
    super.insert(node)

    // balance red black tree
    node.balance()
  }

  // look up red black insert
  private def balance() = {

    if (null == parent) // root
      color = Color.BLACK

    else if (Color.RED == parent) {
      var uncle = getUncle()
      if (Color.RED == uncle) {
        parent.color  = Color.BLACK
        uncle.color   = Color.BLACK
        parent.parent = Color.RED
        balance(parent.parent)
      }
      else {
        if (parent.parent.left == parent) {
          if (parent.left = this) {
            // left left
            // right rotate grandparent
            parent.parent.rotRight()
            // swap parent and grandparent color
            colorSwap(parent, parent.right)
          }
          else {
            // left right
            // left rotate parent
            parent.rotLeft()
            // apply left left on parent
            parent.rotRight()
            colorSwap(this, right)
          }
        }
        else {
          if (parent.left = this) {
            // right left
            // right rotate parent
            parent.rotRight()
            // apply right right on parent
            parent.rotLeft()
            colorSwap(this, left)
          }
          else {
            // right right
            // left rotate grandparent
            parent.parent.rotLeft()
            // swap parent and grandparent color
            colorSwap(parent, parent.left)
          }
        }
      }
    }
  }

  private def getUncle() : rbtree = {
    if (parent.parent.left == parent) {
      return parent.parent.right
    }
    else {
      return parent.parent.left
    }
  }

  private def colorSwap(x : rbtree, y : rbtree) = {
    var t = x.color
    x.color = y.color
    y.color = t
  }

  private def rotRight() = {
    // right rotate g
    //      g         p
    //    p   u  -> x   g
    //   x v           v u
    left.parent = parent
    if (parent.left = this) 
      parent.left = left 
    else 
      parent.right = left
    parent = left 
    left = parent.right
    left.parent = this
    parent.right = this
  }

  private def rotLeft() = {
    // left rotate g
    //      g         p
    //    p   u  <- x   g
    //   x v           v u
    left.parent = parent
    parent.right = left
    left = parent
    parent = parent.parent
    left.parent = this
    if (parent.left = left)
      parent.left = this
    else
      parent.right = this
  }
} 

