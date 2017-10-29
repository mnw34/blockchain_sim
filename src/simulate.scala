/* simulate.scala
 * simulates the blockchain
 */
import blockchain

object hash extends(hashCalc) {
  override get(data : Array[Byte]) : Array[Byte] = {
    java.security.MessageDigest.getInstance("SHA256").digest(data)
  }
}

object simulate {
  def main(args: Array[String]) {
    blockchain b(0xDEADBEEF,  "bsim", hash)
  }
}
