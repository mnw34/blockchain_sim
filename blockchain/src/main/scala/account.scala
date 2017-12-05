// account.scala

import crypto.keypair
import crypto.signable
import crypto.signer
import crypto.verifier
import crypto.hasher
import crypto.sha256

// TODO update to use real signature algorithms
object AccountKeyPairGenerator extends keypair {
  def getPublic(privKey : Array[Byte]) = DumbKeyPair.getPublic(privKey)
}

object AccountSigner extends signer {
  def hashLen                   = DumbSigner.hashLen
  def hash(data : Array[Byte])  = DumbSigner.hash(data)
  def sigLen                    = DumbSigner.sigLen
  def setKey(k : Array[Byte])   = DumbSigner.setKey(k)
  def sign(t : signable)        = DumbSigner.sign(t)
}

object AccountVerifier extends verifier {
  def hashLen                                                       = DumbVerifier.hashLen
  def hash(data : Array[Byte])                                      = DumbVerifier.hash(data)
  def verify(t : signable, pubKey : Array[Byte], sig : Array[Byte]) = DumbVerifier.verify(t, pubKey, sig)
}

object PasswordHasher extends hasher {
  def hashLen = sha256.hashLen
  def hash(data : Array[Byte]) = sha256.hash(data)
}

// an account has an asymmetric key pair, and a balance
class account(val password : String) extends signer with verifier {

  var balance : Int = 0

  var pbkStr : String = ""
  def intToHexString(x : Byte) = { pbkStr += "%02x" format x }
  getPublic.foreach(intToHexString)

  override def toString = password + " [" + balance + "] " + "\npbk " + pbkStr

  // hasher implementations
  def hashLen       = {
    assert(AccountSigner.hashLen == AccountVerifier.hashLen)
    AccountSigner.hashLen
  }
  def hash(data : Array[Byte]) = {
    var h = AccountSigner.hash(data)
    assert(h == AccountVerifier.hash(data))
    h
  }

  // signer implementations
  def sigLen = AccountSigner.sigLen
  def setKey(k : Array[Byte]) = throw new Exception("Attempting to set key on account")
  def sign(t : signable) = AccountSigner.sign(t)

  // verifier implementation
  def verify(t : signable, pubKey : Array[Byte], sig : Array[Byte]) = {
    AccountVerifier.verify(t, pubKey, sig)
  }


  def getPublic = AccountKeyPairGenerator.getPublic(PasswordHasher.hash(password.getBytes))

  def transfer(amt : Int, acc : account) : signedTransfer = {
    if (amt > balance) 
      throw new Exception("Not enough funds for transfer(" + amt + ") balance = " + balance)
    else if (0 == amt)
      throw new Exception("Attempting to transfer 0 amount")
    else if (this == acc)
      throw new Exception("Attempting to transfer to own account")
    else {
      var t = new transfer(amt, this, acc)
      AccountSigner.setKey(PasswordHasher.hash(password.getBytes))
      var sig = t.sign(this)
      if (t.verify(acc, getPublic, sig))
        new signedTransfer(t, sig)
      else
        null
    }
  }
}
