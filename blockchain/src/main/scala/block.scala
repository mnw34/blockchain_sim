/* block.scala
 * This class is used to construct blocks
 * which can be added to a blockchain
 */

import java.security.MessageDigest
import crypto.hashable
import crypto.hasher

class block (val prev : block, val data : hashable, val hasher : hasher) {
  var next : block = null
  val digest : Array[Byte] = hasher.hash(prev.digest ++ data.getData)

  def setNext(n : block) = next = n
  
  override def toString() : String = {
    return "prevHash " + prev + "\n" +
           "data     " + data + "\n" +
           "hash     " + digest + "\n"
  }
}

