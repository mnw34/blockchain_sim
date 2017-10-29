/* blockchain.scala
 * The blockchain begins with a data field containing a name for
 * the blockchain, a nonce previous hash, and a hash calculator
 * instance.
 */
trait hash { 
  def get(data : Array[Byte]) : Array[Byte] 
}

class blockchain(nonce : Int, id : Array[Byte], _hash : hash) {
  val _hash : hash
  var prev : blockchain = Null
  var next : blockchain = Null

  def addBlock(b : block) : Boolean = {
    Boolean retval = False
    assert(null == tail.next)
    if (tail.hash != b.prev.hash)
    assert b.hash == hash.get(tail.hash)(b.data)
  }
}
