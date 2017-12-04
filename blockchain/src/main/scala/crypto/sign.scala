package crypto

// base class for signable types
trait signable extends hashable {
  var signer : signer = null
  var verifier : verifier = null
  def sigLen = signer.sigLen
  def sign(privKey : Array[Byte]) = signer.sign(privKey, this)
  def verify(pubKey : Array[Byte], sig : Array[Byte]) = verifier.verify(pubKey, this, sig)
}


// interface definition for all signature algorithms
trait signer {
  def sigLen : Int // convenience function for getting signature length
  def sign(privKey : Array[Byte], t : signable) : Array[Byte] // return signature
}

trait verifier {
  def verify(pubKey : Array[Byte], t : signable, sig : Array[Byte]) : Boolean
}

// base class for keypair types
abstract class keypair(val privKey : Array[Byte]) {
  def getPublic : Array[Byte]
}

class dumbKeyPair(privKey : Array[Byte]) extends keypair(privKey) {
  def getPublic = privKey // this dumb key pair just is XOR, need real asymmetric 
}

object DumbSigner extends signer {
  def sign(privKey : Array[Byte], t : signable) = {
    // just hash the data and xor with the private key for the signature
    var hash = t.getHash
    var sig = new Array[Byte](hash.length)
    for (i <- 0 until sig.length)
      sig(i) = (hash(i) ^ privKey(i % privKey.length)).toByte
    sig
  }
}

object DumbVerifier extends verifier {
  def verify(pubKey : Array[Byte], t : hashable, sig : Array[Byte]) = {
    if (t.hashLen != sig.length)
      false
    else {
      var hash = t.getHash
      var good = true
      for (i <- 0 until sig.length)
        if (sig(i) != (hash(i) ^ pubKey(i % pubKey.length)).toByte)
          good = false
      good 
    }
  }
}

// ECDSA-P239 singleton
object ecdsa_P239 extends signer with verifier {
  
  // TODO
  /*
  KeyPairGenerator kpg = KeyPairGenerator.getInstance("EC")
  AlgorithmParameterSpec keySpec = 
  kpg.initialize( 
  Signature ecdsaSign = Signature.getInstance("SHA256withECDSA", "BC");
ecdsaSign.initSign(pair.getPrivate());
  def get(data : Array[Byte]) = java.security.MessageDigest.getInstance("SHA256").digest(data)
  */
}


