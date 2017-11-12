// bstree.scala
// binary search tree

import scala.reflect.ClassTag
import scala.reflect.ClassManifest

// takes a datatype and compare function
class bstree[T](_t : T, lessThan : (T, T) => Boolean) {
  var right  : bstree[T] = null
  var left   : bstree[T] = null
  var parent : bstree[T] = null
  var t      : T = _t
  
  def insert(node : bstree[T]) : Unit = {
    if (true == lessThan(node.t, t)) {

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
    var node = new bstree(t, lessThan)
    this.insert(node)
  }

  def find(t : T) : bstree[T] = {
    var r : bstree[T] = this
    if (r.t == t) 
      return r
    else if (lessThan(t, r.t)) {
      if (null != left)
        return left.find(t)
      else 
        return null
    }
    else {
      if (null != right) 
        return right.find(t)
      else 
        return null
    }
  }

  def delete() : bstree[T] = {
    if (left != null && right != null) { // two children
      var m = right.min()
      if (null != parent) {
        if (this == parent.left) parent.left = m
        else parent.right = m
      }
      if (this != m.parent) m.parent.left = m.right
      m.parent = parent
      m.left = left
      left.parent = m
      return m
    }
    else if (left != null) { // left only
      left.parent = parent
      if (null != parent) {
        if (this == parent.right) parent.right = left
        else parent.left = left
      }
      return left
    }
    else if (right != null) { // right only
      right.parent = parent
      if (null != parent) {
        if (this == parent.right) parent.right = right
        else parent.left = right
      }
      return right
    }
    else // no children
    {
      if (this == parent.right) parent.right = null
      else parent.left = null
      return null
    }
  }
 
  def inorder(l : Array[T])(implicit m : ClassTag[T]) : Array[T] = {
    var tmp = l
    if (null != left) tmp = left.inorder(tmp)

    tmp = tmp :+ t

    if (null != right) tmp = right.inorder(tmp)

    return tmp
  }

  def preorder(l : Array[T])(implicit m : ClassTag[T]) : Array[T] = {
    var tmp = l :+ t
    if (null != left)
      tmp = left.preorder(tmp)
    if (null != right)
      tmp = right.preorder(tmp)

    return tmp
  }

  def postorder(l : Array[T])(implicit m : ClassTag[T]) : Array[T] = {
    var tmp = l
    if (null != left)
      tmp = left.postorder(tmp)
    if (null != right)
      tmp = right.postorder(tmp)

    return tmp :+ t
  }

  private def min() : bstree[T] = {
    if (left == null)
      return this
    else
      return left.min()
  }
}
