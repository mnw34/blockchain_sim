package crypto

// base class for signable types
trait signable extends hashable {
  def sign(signer : signer) = signer.sign(this)
  def verify(verifier : verifier, pubKey : Array[Byte], sig : Array[Byte]) : Boolean = {
    verifier.verify(this, pubKey, sig)
  }
}

// interface definitions for all signature algorithms
trait signer extends hasher {
  def sigLen : Int // convenience function for getting signature length
  def setKey(k : Array[Byte]) : Unit   // must be called before each call to sign
  def sign(t : signable) : Array[Byte] // return signature
}

trait verifier extends hasher {
  def verify(t : signable, pubKey : Array[Byte], sig : Array[Byte]) : Boolean
}

// base class for keypair types
abstract class keypair {
  def getPublic(privKey : Array[Byte]) : Array[Byte]
}

// ECDSA-P239 singleton
//object ecdsa_P239 extends signer with verifier {
  
  // TODO
  /*
  KeyPairGenerator kpg = KeyPairGenerator.getInstance("EC")
  AlgorithmParameterSpec keySpec = 
  kpg.initialize( 
  Signature ecdsaSign = Signature.getInstance("SHA256withECDSA", "BC");
ecdsaSign.initSign(pair.getPrivate());
  def get(data : Array[Byte]) = java.security.MessageDigest.getInstance("SHA256").digest(data)
  */
//}


