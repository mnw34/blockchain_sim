// bstree.scala
// binary search tree

// takes a datatype and compare function
class bstree[T](var value : T, val lessThan : (T, T) => Boolean, val equals : (T, T) => Boolean) {
  var right  : bstree[T] = null
  var left   : bstree[T] = null
  var parent : bstree[T] = null
  
  def insert(node : bstree[T]) : Unit = {
    if (true == lessThan(node.value, value)) {

      if (null == left) {
        left = node
        node.parent = this
      }
      else left.insert(node)
    }

    else if (null == right) {
      right = node
      node.parent = this
    }
    else right.insert(node)
  }

  def insert(t : T) : Unit = {
    var node = new bstree(t, lessThan, equals)
    this.insert(node)
  }

  def root : bstree[T] = if (null == parent) this else parent.root

  def height : Int = {
    if (null == left && null == right) 1      // height of this
    else if (null == left) 1 + right.height
    else if (null == right) 1 + left.height
    else {
      var lh = left.height
      var rh = right.height
      if (lh > rh) 1 + lh
      else         1 + rh
    }
  }

  def find(v : T) : bstree[T] = {
    var r : bstree[T] = this
    if (v.equals(r.value))
      return r
    else if (lessThan(v, r.value)) {
      if (null != left)
        return left.find(v)
      else 
        return null
    }
    else {
      if (null != right) 
        return right.find(v)
      else 
        return null
    }
  }

  // returns the node now at this index in the tree
  def delete : bstree[T] = {
    if (left != null && right != null) { // two children
      var m = right.min()
      if (null != parent) {
        if (this == parent.left) parent.left = m
        else parent.right = m
      }
      if (this != m.parent) {
        m.parent.left = m.right
        if (null != m.right) m.right.parent = m.parent
      }
      m.parent = parent
      m.left = left
      if (m != right) {
        m.right = right
        right.parent = m
      }
      left.parent = m
      return m
    }
    else if (left != null) { // left only
      if (null != parent) {
        if (this == parent.right) parent.right = left
        else parent.left = left
      }
      left.parent = parent
      return left
    }
    else if (right != null) { // right only
      if (null != parent) {
        if (this == parent.right) parent.right = right
        else parent.left = right
      }
      right.parent = parent
      return right
    }
    else // no children
    {
      if (null == parent) return null
      else if (this == parent.right) parent.right = null
      else parent.left = null
      return null
    }
  }
 
  def inorder(f : bstree[T] => Unit) : Unit = {
    if (null != left)
      left.inorder(f)
    f(this)
    if (null != right)
      right.inorder(f)
  }

  def preorder(f : bstree[T] => Unit) : Unit = {
    f(this)
    if (null != left)
      left.preorder(f)
    if (null != right)
      right.preorder(f)
  }

  def postorder(f : bstree[T] => Unit) : Unit = {
    if (null != left)
      left.postorder(f)
    if (null != right)
      right.postorder(f)
    f(this)
  }

  private def min() : bstree[T] = {
    if (left == null)
      return this
    else
      return left.min()
  }
}
