// MerkelTree.scala

object hashFunctions {
  // return -1 0 or 1 for _x less than _y, equal to or greater than
  def hashCompare(_x: hashable, _y: hashable) = {
    var i = 0
    var x = _x.getHash
    var y = _y.getHash
    var maxEnd = if (x.length < y.length) x.length else y.length
    while (i < maxEnd && x(i) == y(i)) i += 1
    if (i == maxEnd) 0
    else if (x(i) < y(i)) -1
    else 1
  }

  def hashLessThan(x: hashable, y: hashable) = {
    (-1 == hashCompare(x, y))
  }

  def hashEquals(x: hashable, y: hashable) = {
    (0 == hashCompare(x, y))
  }
}

class MerkelTree(var value : hashable) {
  var left   : MerkelTree = null
  var right  : MerkelTree = null
  var parent : MerkelTree = null
  var numDescendants = 0 // number of descendants from this node

  // helper public interface to the merkel tree
  def getHash = value.getHash

  def isFull : Boolean = {
    if (null == left && null == right)      // leaf is full
      true
    else if (null != left && null != right) // recurse
      left.isFull && right.isFull
    else
      false // one child
  }

  // traverses up the tree recalculating all hashes and returning root
  // should never be called on a leaf node, must have at least left child
  def merkel : MerkelTree = {
    // combined hash
    var rHash = if (null == right) left.getHash else right.getHash
    value.data = value.hashCalc.get(left.getHash ++ rHash)
    if (null == parent) this
    else parent.merkel
  }

  // returns the new root of the tree
  def insert(v : hashable) : MerkelTree = {
    if (null == left) {
      left = new MerkelTree(v)
      numDescendants += 1
      merkel
    }
    else if (null == right) {
      right = new MerkelTree(v)
      numDescendants += 1
      merkel
    }
    else if (!left.isFull) {
      left.insert(v)
      this
    }
    else if (!right.isFull) {
      right.insert(v)
      this
    }
    else {
      // need to create a new root and right subtree
      var newRoot  = new MerkelTree(v)
      var rSubtree = new MerkelTree(v)
      this.parent = newRoot
      newRoot.left = this
      newRoot.right = rSubtree
      newRoot.merkel
    }
  }
}
