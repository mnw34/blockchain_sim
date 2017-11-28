// rbtree.scala
// red black tree

// define red black tree colors
class Color
object BLACK extends Color { override def toString = "b" }
object RED extends Color { override def toString = "r" }
object DBLACK extends Color { override def toString = "db" }


class rbtree[T](t : T, lessThan : (T, T) => Boolean, equals : (T, T) => Boolean)
  extends bstree(t, lessThan, equals) {

  var color  : Color = BLACK

  private def asRb(n : bstree[T]) = n.asInstanceOf[rbtree[T]]

  private def setColor(n : bstree[T], c : Color) = asRb(n).color = c
  private def getColor(n : bstree[T]) = asRb(n).color

  implicit class bstreeColor(n : bstree[T]) {
    def recolor = asRb(n).recolor
    def rotRight = asRb(n).rotRight
    def rotLeft = asRb(n).rotLeft
    def balance = asRb(n).balance
  }

  override def root: rbtree[T] = asRb(super.root)

  def insert(node : rbtree[T]) = {
    // first step is color inserted node red
    node.color = RED

    // call the binary search tree insert
    super.insert(node)

    // balance red black tree
    node.balance
  }

  override def find(t : T) : rbtree[T] = asRb(super.find(t))

  override def delete : rbtree[T] = {

    var r = asRb(super.delete)

    // double black replacement node
    if (BLACK == color && (null == r || BLACK == r.color)) {
      r.color = DBLACK
      r.recolor
    }
    else {
      r.color = BLACK
    }
    r // return
  }

  private def recolor : Unit = {
    if (null == parent) color = BLACK
    else {
      // sibling
      var s : rbtree[T] = if (this == parent.left) asRb(parent.right) else asRb(parent.left)
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
              s.left.rotRight
              s.color = RED
              setColor(s.parent, BLACK)
              s.parent.rotLeft
              s.color = BLACK
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

    if (null == parent) // root
      color = BLACK

    else if (RED == getColor(parent)) {
      var uncle = getUncle()
      if (null == uncle || BLACK == uncle.color) { // black uncle
        if (parent.parent.left == parent) {
          if (parent.left == this) {
            // left left
            // right rotate grandparent
            parent.parent.rotRight
            // swap parent and grandparent color
            colorSwap(parent, parent.right)
          }
          else {
            // left right
            // left rotate parent
            parent.rotLeft
            // apply left left on parent
            parent.rotRight
            colorSwap(this, right)
          }
        }
        else {
          if (parent.left == this) {
            // right left
            // right rotate parent
            parent.rotRight
            // apply right right on parent
            parent.rotLeft
            colorSwap(this, left)
          }
          else {
            // right right
            // left rotate grandparent
            parent.parent.rotLeft
            // swap parent and grandparent color
            colorSwap(parent, parent.left)
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

  private def getUncle() : rbtree[T] = {
    if (null == parent || null == parent.parent)
      null
    else if (parent.parent.left == parent)
      asRb(parent.parent.right)
    else
      asRb(parent.parent.left)
  }

  private def colorSwap(x : bstree[T], y : bstree[T]) : Unit = {
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

