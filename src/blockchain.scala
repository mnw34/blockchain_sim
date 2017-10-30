/* blockchain.scala
 * The blockchain begins with a data field containing a name for
 * the blockchain, a nonce previous hash, and a hash calculator
 * instance.
 */
trait hash { 
  def get(data : Array[Byte]) : Array[Byte] 
}

class blockchain(nonce : Int, data : Array[Byte], _hash : hash) {
  var prev : blockchain = Null
  var next : blockchain = Null
  val data : Array[Byte] = data

  def addBlock(b : blockchain) : Boolean = {
    assert b.hash == _hash.get(.hash)(b.data)
  }
}
