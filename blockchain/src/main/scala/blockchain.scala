/* blockchain.scala
 * The blockchain begins with a data field containing a name for
 * the blockchain, a nonce previous hash, and a hash calculator
 * instance.
 */
class blockchain(nonce : Int, d : Array[Byte], hash : hash) {
  var prev : blockchain = null
  var next : blockchain = null
  val data : Array[Byte] = d

//  def addBlock(b : blockchain) : Boolean = {
 //   assert b.hash == _hash.get(.hash)(b.data)
  //}
}
