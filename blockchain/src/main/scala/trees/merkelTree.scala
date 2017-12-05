// merkelTree.scala

import crypto.hashable
import crypto.hasher
import crypto.sha256

// inner node data type containing only a combined hash of child nodes
class merkelTreeHash(var x : hashable, var y : hashable) extends hashable {
  var data = MerkelTreeHasher.hash(x.getData ++ y.getData)
  def getData = data
  def setData(data : Array[Byte]) = this.data = data
}

object MerkelTreeHasher extends hasher {
  def hashLen = sha256.hashLen
  def hash(data : Array[Byte]) = sha256.hash(data)
}


class merkelTree(t : hashable)
  extends bst[hashable](t, null, null) with hashable
{
  // hashable implementation
  def getData = value.getData
  def setData(data : Array[Byte]) = value.setData(data)

  var numDescendants = 0 // number of descendants from this node

  implicit def bstToMerkel(node : bst[hashable]) : merkelTree = node.asInstanceOf[merkelTree]

  override def delete(v: hashable): bst[hashable] = throw new Exception("Cannot delete from Merkel tree")

  // traverses up the tree recalculating all hashes and returning root
  // must have left and right child
  def merkel : merkelTree = {

    // combined hash of children creates inner node data
    var t = new merkelTreeHash(left.value, right.value)
    value.setData(t.getData)

    numDescendants = 2 + left.numDescendants + right.numDescendants

    if (null == parent) this
    else parent.merkel
  }

  // temp data holder before call to merkel with a back reference to the node
  class merkelInnerNodeData extends hashable {
    var backRef : merkelTree = null
    var x : Array[Byte] = null
    def getData = x
    def setData(data : Array[Byte]) = x = data
    override def toString = {
      var r = "inner <"
      if (backRef.left.isLeaf) {
         r += " left " + backRef.left.value
         if (backRef.right.isLeaf)
           r += " right " + backRef.right.value

      }
      else if (backRef.right.isLeaf) {
        r += "left inner right " + backRef.right.value
      }
      else {
      }
      r + " >"
    }
  }

  // returns the new root of the tree
  def insert(node : merkelTree) : merkelTree = {
    if (null == left) {
      if (null != right) throw new Exception("Invalid Merkel Tree with left child only")

      // create new parent
      var t = new merkelInnerNodeData
      var p = new merkelTree(t)
      t.backRef = p

      // new node goes in left subtree in case it has children
      // left height > right height is allowed
      p.left = node
      node.parent = p
      p.right = this
      p.parent = parent
      setNode(parent, p)
      parent = p

      p.merkel
    }
    else if (left.numDescendants > right.numDescendants) {
      right.asInstanceOf[merkelTree].insert(node)
    }
    else if (null != parent && numDescendants < getSibling.numDescendants) {
      left.asInstanceOf[merkelTree].insert(node)
    }
    else { // full tree alread

      if (right.height != left.height) throw new Exception("Invalid Merkel tree with heavy right")

      // need to create a new parent and right subtree
      var t = new merkelInnerNodeData
      var p = new merkelTree(t)
      t.backRef = p
      p.right = node
      node.parent = p
      p.left = this
      p.parent = parent
      parent = p

      p.merkel
    }
  }

  override def find(v : hashable) : merkelTree = {
    if (isLeaf && v.getData.sameElements(value.getData))
      return this
    else {
      // check for collisions
      var l = if (null == left) null else left.find(v)
      var r = if (null == right) null else right.find(v)
      if (null != l) l else r
    }
  }
}
