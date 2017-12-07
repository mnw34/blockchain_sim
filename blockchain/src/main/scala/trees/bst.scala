// bstree.scala
// binary search tree

import scala.collection.mutable.ArrayBuffer

// takes a datatype and compare function
class bst[T](var value : T, val lessThan : (T, T) => Boolean, val equals : (T, T) => Boolean) {
  var right  : bst[T] = null
  var left   : bst[T] = null
  var parent : bst[T] = null

  def isLeaf = if (null == left && null == right) true else false


  def insert(node : bst[T]) : Unit = {
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
    var node = new bst(t, lessThan, equals)
    this.insert(node)
  }

  def root : bst[T] = if (null == parent) this else parent.root

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

  def find(v : T) : bst[T] = {
    var r : bst[T] = this
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

  // find the adjacent node and set to new node
  protected def setNode(adj : bst[T], node : bst[T]) = {
    if (null == adj) {
      // just return for null association
    }
    else if (adj == parent) {
      if (this == parent.left)
        parent.left = node
      else
        parent.right = node

      if (null != node)
        node.parent = parent
    }
    else if (adj == right) {
      right.parent = node
      if (null != node)
        node.right = right
    }
    else if (adj == left) {
      left.parent = node
      if (null != node)
        node.left = left
    }
    else throw new Exception("Invalid adjacent node in setNode.")
  }

  // returns the root or null for empty tree
  def delete(v : T) : bst[T] = {
    if (value.equals(v)) { // delete this
      if (left != null && right != null) { // two children
        // get successor
        var m = right.successor()
        value = m.value
        m.delete(value)
        this
      }
      else if (left != null) { // left only
        setNode(parent, left) // swap left
        left
      }
      else if (right != null) { // right only
        setNode(parent, right) // swap right
        right
      }
      else // no children
      {
        null // clear the caller reference to this node
      }
    }
    else if (lessThan(value, v)) {
      if (null != right) this.right = right.delete(v)
      this
    }
    else {
      if (null != left) this.left = left.delete(v)
      this
    }
  }

  def asList = {
    var l = new ArrayBuffer[T](5)
    var i = 0
    var f = (t : bst[T]) => {
      if (null == t) {
        // return
      }
      else if (i < l.length) {
        l(i) = t.value
        i += 1
      }
      else {
        var u = new ArrayBuffer[T](l.length * 2)
        u.prependAll(l)
        l = u
        l.prepend(t.value)
        i += 1
      }
    }
    inorder(f)
    if (null != l) l.toList else null // return array of accounts
  }
 
  def inorder(f : bst[T] => Unit) : Unit = {
    if (null != left)
      left.inorder(f)
    f(this)
    if (null != right)
      right.inorder(f)
  }

  def preorder(f : bst[T] => Unit) : Unit = {
    f(this)
    if (null != left)
      left.preorder(f)
    if (null != right)
      right.preorder(f)
  }

  def postorder(f : bst[T] => Unit) : Unit = {
    if (null != left)
      left.postorder(f)
    if (null != right)
      right.postorder(f)
    f(this)
  }

  protected def successor() : bst[T] = {
    if (left == null)
      return this
    else
      return left.successor()
  }

  protected def getSibling() = {
    if (null == parent)
      null
    else if (this == parent.left)
      parent.right
    else
      parent.left
  }

}
