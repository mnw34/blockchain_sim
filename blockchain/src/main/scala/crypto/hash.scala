package crypto

// base class for hashable types
trait hashable {
  var hasher : hasher = null
  def hashLen = hasher.hashLen
  def getData : Array[Byte]
  def getHash = hasher.getHash(getData)
}

// interface definition for all hashes
trait hasher {
  def hashLen : Int
  def getHash(data : Array[Byte]) : Array[Byte]
  def getHash(t : hashable) : Array[Byte] = getHash(t.getData)
}

// SHA-256 singleton
object sha256 extends hasher {
  def hashLen = 32
  def getHash(data : Array[Byte]) = java.security.MessageDigest.getInstance("SHA256").digest(data)
}


