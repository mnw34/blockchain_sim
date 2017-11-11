/* block.scala
 * This class is used to construct blocks
 * which can be added to a blockchain
 */

import java.security.MessageDigest

class block (p : block, d : Array[Byte], hash : hash) {
  val prev : block = p
  var next : block = null
  var data : Array[Byte] = d
  val digest : Array[Byte] = hash.get(hash.get(prev.data))

  def setNext(n : block) = next = n
  
  override def toString() : String = {
    return "prevHash " + prev + "\n"
           "data     " + data + "\n"
           "hash     " + digest + "\n"
  }
}

