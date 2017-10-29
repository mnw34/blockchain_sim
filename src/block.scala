/* block.scala
 * This class is used to construct blocks
 * which can be added to a blockchain
 */

import java.security.MessageDigest

class block (prev : block, data : Array[Byte], hasher : hashCalc) {
  val prev : block = prev
  var next : block = null
  val hash : Array[Byte] = hasher.hash(prev.hash)(data) /* curried function call */

  def setNext(next : block) : Unit = { next = next }
  
  override def toString() : String = {
    return "prevHash " + this.prevHash + "\n"
           "data     " + this.data + "\n"
           "hash     " + this.hash + "\n"
  }
}

object hashCalc extends hashCalc {
  val md = MessageDigest.getInstance("SHA256")
  def hash(data : Array[Byte]) : Array[Byte] = {
    return md.digest(data)
  }
}
