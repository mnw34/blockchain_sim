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

      if (null == left) left = node

      else left.insert(node)
    }

    else if (null == right) right = node

    else right.insert(node)
  }

  def insert(t : T) : Unit = {
    var node = new bstree(t, lessThan)
    this.insert(node)
  }

  def inorder(l : Array[T])(implicit m : ClassTag[T]) : Array[T] = {
    var tmp = l
    if (null != left) tmp = left.inorder(tmp)

    tmp :+ t

    if (null != right) tmp = right.inorder(tmp)

    return tmp
  }

  def preorder(l : Array[T])(implicit m : ClassTag[T]) : Array[T] = {
    l :+ t
    var tmp = l
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
}
