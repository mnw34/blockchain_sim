// rbtree.scala
// red black tree
object Color {
  val RED    = 0
  val BLACK  = 1
  val DBLACK = 2
}

class rbtree[T](t : T, lessThan : (T, T) => Boolean)
  extends bstree(t, lessThan) {

  var color  : Int = Color.BLACK

  def insert(node : rbtree[T]) : Unit = {
    // first step is color inserted node red
    node.color = Color.RED

    // call the binary search tree insert
    super.insert(node)

    // balance red black tree
    node.balance()
  }

  private def asRb(t : bstree[T]) = t.asInstanceOf[rbtree[T]]

  private def setColor(x : bstree[T], c : Int) = asRb(x).color = c

  // look up red black insert
  private def balance() : Unit = {

    if (null == parent) // root
      color = Color.BLACK

    else if (Color.RED == asRb(parent).color) {
      var uncle = getUncle()
      if (Color.RED == asRb(uncle).color) {
        setColor(parent, Color.BLACK)
        setColor(uncle,  Color.BLACK)
        setColor(parent.parent, Color.RED)
        asRb(parent.parent).balance()
      }
      else {
        if (parent.parent.left == parent) {
          if (parent.left == this) {
            // left left
            // right rotate grandparent
            asRb(parent.parent).rotRight()
            // swap parent and grandparent color
            colorSwap(parent, parent.right)
          }
          else {
            // left right
            // left rotate parent
            asRb(parent).rotLeft()
            // apply left left on parent
            asRb(parent).rotRight()
            colorSwap(this, right)
          }
        }
        else {
          if (parent.left == this) {
            // right left
            // right rotate parent
            asRb(parent).rotRight()
            // apply right right on parent
            asRb(parent).rotLeft()
            colorSwap(this, left)
          }
          else {
            // right right
            // left rotate grandparent
            asRb(parent.parent).rotLeft()
            // swap parent and grandparent color
            colorSwap(parent, parent.left)
          }
        }
      }
    }
  }

  private def getUncle() : bstree[T] = {
    if (parent.parent.left == parent) {
      return parent.parent.right
    }
    else {
      return parent.parent.left
    }
  }

  private def colorSwap(x : bstree[T], y : bstree[T]) : Unit = {
    var _x = asRb(x)
    var _y = asRb(y)
    var t = _x.color
    _x.color = _y.color
    _y.color = t
  }

  private def rotRight() : Unit = {
    // right rotate g
    //      g         p
    //    p   u  -> x   g
    //   x v           v u
    left.parent = parent
    if (parent.left == this) 
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
    if (parent.left == left)
      parent.left = this
    else
      parent.right = this
  }
} 

