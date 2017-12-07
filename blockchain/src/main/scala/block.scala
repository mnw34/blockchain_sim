// block.scala

import crypto.{hashable, hasher}

class rootBlock(hasher : hasher, st : signedTransfer) extends block(null, hasher, st) {
  override val seqNum : Int = 1

  // just make an unitialized hash for the root block
  override val prevHash : Array[Byte] = new Array[Byte](hasher.hashLen)
}

class block(val prev : block, val hasher : hasher, val st : signedTransfer) extends hashable {
  var nonce  : Int = 0
  val seqNum : Int = prev.seqNum + 1

  val prevHash : Array[Byte] = prev.getHash(hasher)

  override def getData = nonce.toHexString.getBytes  ++
                         seqNum.toHexString.getBytes ++
                         prevHash

  override def setData(data : Array[Byte]) =
    throw new Exception("Attempting to set data on block object")
}
