import crypto.signable
import crypto.keypair
import crypto.signer
import crypto.verifier
import crypto.sha256

object DumbKeyPair extends keypair {
  def getPublic(privKey : Array[Byte]) = privKey // this dumb key pair just is XOR, need real asymmetric
}

object DumbSigner extends signer {
  var pvk : Array[Byte] = null
  def hashLen = sha256.hashLen
  def hash(data : Array[Byte]) = sha256.hash(data)
  def sigLen = hashLen
  def setKey(k : Array[Byte]) = pvk = k
  def sign(t : signable) = {
    // just hash the data and xor with the private key for the signature
    var hash = t.getHash(this)
    var sig = new Array[Byte](hashLen)
    for (i <- 0 until sigLen)
      sig(i) = (hash(i) ^ pvk(i % pvk.length)).toByte

    pvk = null
    sig
  }
}

object DumbVerifier extends verifier {
  def hashLen = sha256.hashLen
  def hash(data : Array[Byte]) = sha256.hash(data)
  def verify(t : signable, pubKey : Array[Byte], sig : Array[Byte]) = {
    if (hashLen != sig.length)
      false
    else {
      var hash = t.getHash(this)
      var good = true
      for (i <- 0 until sig.length)
        if (sig(i) != (hash(i) ^ pubKey(i % pubKey.length)).toByte)
          good = false
      good 
    }
  }
}

