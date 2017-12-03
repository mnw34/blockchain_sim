// rbtree.scala
// red black tree

// define red black tree colors
class Color
object BLACK extends Color { override def toString = "b" }
object RED extends Color { override def toString = "r" }
object DBLACK extends Color { override def toString = "db" }

class RBT[T](t : T, lessThan : (T, T) => Boolean, equals : (T, T) => Boolean)
  extends BST(t, lessThan, equals) {

  var color  : Color = BLACK

  private def asRb(n : BST[T]) : RBT[T] = {
    if (null == n)
      null
    else
      n.asInstanceOf[RBT[T]]
  }

  private def getColor(n : BST[T]) = {
    if (null == n) BLACK
    else asRb(n).color
  }

  private def setColor(n : BST[T], c : Color) = {
    if (null == n) { // if leaf
      // always black
    }
    else {
      if (DBLACK == c && RED == getColor(n))
        asRb(n).color = BLACK // double black + red = black
      else
        asRb(n).color = c
    }
  }

  implicit class bstreeColor(n : BST[T]) {
    def recolor = asRb(n).recolor
    def rotRight = asRb(n).rotRight
    def rotLeft = asRb(n).rotLeft
    def balance = asRb(n).balance
  }

  override def root: RBT[T] = asRb(super.root)

  def insert(node : RBT[T]) = {
    // first step is color inserted node red
    node.color = RED

    // call the binary search tree insert
    super.insert(node)

    // balance red black tree
    node.balance

    // return root node
    root
  }

  override def find(v : T) : RBT[T] = asRb(super.find(v))

  protected def reduce : Unit = {
    if (null == parent) color = BLACK
    else if (DBLACK != color) {
      // not double black, do nothing
    }
    else {
      var sibling = getSibling()
      if (RED == getColor(sibling)) {
        if (sibling == parent.left) {
          parent.rotRight
          swapColor(parent, parent.parent)
          reduce
        }
        else {
          parent.rotLeft
          swapColor(parent, parent.parent)
          reduce
        }
        color = BLACK
      }
      // black sibling and nephews
      else if (null == sibling ||
        (BLACK == getColor(sibling.left) && (BLACK == getColor(sibling.right))))
      {
        setColor(sibling, RED)
        color = BLACK
        setColor(parent, DBLACK)
        asRb(parent).reduce
      }
      else { // black sibling and at least one red nephew
        if (sibling == parent.right) {
          if (BLACK == getColor(sibling.right)) { // right left
            sibling.rotRight
            swapColor(sibling, sibling.parent)
            sibling = getSibling()
          } // continue right right

          // right right
          parent.rotLeft
          setColor(parent.parent, getColor(parent))
          setColor(parent.parent.right, BLACK)
          color = BLACK
          setColor(parent, BLACK)
        }
        else {
          if (BLACK == getColor(sibling.left)) { // left right
            sibling.rotLeft
            swapColor(sibling, sibling.parent)
            sibling = getSibling()
          } // continue left left

          // left left
          parent.rotRight
          setColor(parent.parent, getColor(parent))
          setColor(parent.parent.left, BLACK)
          color = BLACK
          setColor(parent, BLACK)
        }
      }
    }
  }

  override def delete(v : T) : RBT[T] = {
    if (value.equals(v) && (null == left || null == right)) {

      var n = if (null != left) left else right

      if (null == parent) {
        setColor(n, BLACK)
        if (null != n) n.parent = null
        return asRb(n)
      }
      else if (RED == color || RED == getColor(n)) {
        setColor(n, BLACK)
        setNode(parent, n)
      }
      else if (null == n) { // deleting leaf
        color = DBLACK
        reduce
        setNode(parent, n)
      }
      else {
        setNode(parent, n)
        setColor(n, DBLACK)
        asRb(n).reduce
      }
    }
    // normal BST
    else if (value.equals(v)) { // delete this
      // get successor
      var m = right.successor()
      value = m.value
      asRb(m).delete(value)
    }
    else if (lessThan(value, v)) {
      if (null != right)
        asRb(right).delete(v)
    }
    else {
      if (null != left)
        asRb(left).delete(v)
    }
    root // return root
  }

  private def recolor : Unit = {
    if (null == parent) color = BLACK
    else {
      // sibling
      var s : RBT[T] = if (this == parent.left) asRb(parent.right) else asRb(parent.left)
      if (BLACK == s.color) { // black sibling
        // black sibling children
        if ((null == s.left || BLACK == getColor(s.left)) && (null == s.right || BLACK == getColor(s.right))) {
          s.color = RED
          if (BLACK == getColor(parent)) {
            setColor(parent, DBLACK)
            parent.recolor
          }
          else setColor(parent, BLACK)
        }
        else { // at least one red sibling child
          if (s == parent.left) { // left sibling
            if (BLACK == getColor(s.left)) { // left right
              s.right.rotLeft
              s.color = RED
              setColor(s.parent, BLACK)
              s.parent.rotRight
              s.color = BLACK
            }
            else { // left left
              s.rotRight
              setColor(s.left, BLACK)
            }
          }
          else { // right sibling
            if (BLACK == getColor(s.right)) { // right left
              s.rotRight
              setColor(s.parent, BLACK)
              s.parent.rotLeft
            }
            else { // right right
              s.rotLeft
              setColor(s.right, BLACK)
            }
          }
          if (s == parent.right && BLACK == getColor(s.right)) { // right left case
            setColor(s.left, BLACK)
            s.left.rotRight
            s.parent.rotLeft
          }
          else { // left left, left right, right right cases
            s.rotLeft
            setColor(s.right, BLACK)
          }
        }
      }
      else { // red sibling

      }
    }
  }

  // look up red black insert
  private def balance() : Unit = {

    if (null == parent) { // root
      color = BLACK
    }
    else if (RED == getColor(parent)) {
      var uncle = getUncle()
      if (null == uncle || BLACK == uncle.color) { // black uncle
        if (parent.parent.left == parent) {
          if (parent.left == this) {
            // left left
            // right rotate grandparent
            parent.parent.rotRight
            // swap parent and grandparent color
            swapColor(parent, parent.right)
          }
          else {
            // left right
            // left rotate parent
            parent.rotLeft
            // apply left left on parent
            parent.rotRight
            swapColor(this, right)
          }
        }
        else {
          if (parent.left == this) {
            // right left
            // right rotate parent
            parent.rotRight
            // apply right right on parent
            parent.rotLeft
            swapColor(this, left)
          }
          else {
            // right right
            // left rotate grandparent
            parent.parent.rotLeft
            // swap parent and grandparent color
            swapColor(parent, parent.left)
          }
        }
      }
      else { // red uncle
        setColor(parent, BLACK)
        uncle.color = BLACK
        setColor(parent.parent, RED)
        parent.parent.balance
      }
    }
  }

  private def getUncle() = {
    if (null == parent || null == parent.parent)
      null
    else if (parent.parent.left == parent)
      asRb(parent.parent.right)
    else
      asRb(parent.parent.left)
  }

  private def getSibling() = {
    if (null == parent)
      null
    else if (this == parent.left)
      asRb(parent.right)
    else
      asRb(parent.left)
  }

  private def swapColor(x : BST[T], y : BST[T]) = {
    var t = asRb(x).color
    setColor(x, getColor(y))
    setColor(y, t)
  }

  private def rotRight() : Unit = {
    // right rotate g
    //      g         p
    //    p   u  -> x   g
    //   x v           v u
    left.parent = parent
    if (null != parent) {
      if (parent.left == this)
        parent.left = left
      else
        parent.right = left
    }
    parent = left
    left = parent.right
    parent.right = this
    if (null != left) left.parent = this
  }

  private def rotLeft() = {
    // left rotate p
    //      g         p
    //    p   u  <- x   g
    //   x v           v u
    right.parent = parent
    if (null != parent) {
      if (parent.left == this)
        parent.left = right
      else
        parent.right = right
    }
    parent = right
    right = parent.left
    parent.left = this
    if (null != right) right.parent = this
  }
} 

