package crypto

// base class for hashable types
trait hashable {
  def getData : Array[Byte]
  def setData(data : Array[Byte]) : Unit
  def getHash(hasher : hasher) = hasher.hash(this)
}

// interface definition for all hashes
trait hasher {
  def hashLen : Int
  def hash(data : Array[Byte]) : Array[Byte]
  def hash(t : hashable) : Array[Byte] = hash(t.getData)
}

// SHA-256 singleton
object sha256 extends hasher {
  val _sha256 = java.security.MessageDigest.getInstance("SHA-256")
  def hashLen = 32
  def hash(data : Array[Byte]) = _sha256.digest(data)
}
