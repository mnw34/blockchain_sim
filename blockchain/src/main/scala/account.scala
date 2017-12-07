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

// account holders create transactions to send an amount to a public account
class public_account(var pubKey : Array[Byte], _balance : Int, _prevTransferSeqNum : Int) extends account("password") {

  balance = _balance
  prevTransferSeqNum = _prevTransferSeqNum

  override def sign(t : signable) =
    throw new Exception("Attempting to sign with public account")

  override def getPublic = pubKey
}

// an account has an asymmetric key pair, and a balance
class account(val password : String) extends signer with verifier {

  var balance : Int = 0
  var inProcessTransferAmount : Int = 0
  var prevTransferSeqNum : Int = 0

  var pbkStr : String = ""
  def byteToHexString(x : Byte) = { pbkStr += "%02x" format x }
  getPublic.foreach(byteToHexString)

  override def toString = "PW (" + password + ") [$" + balance + "] PreviousSeqNum " + prevTransferSeqNum + " PBK <" + pbkStr + ">"

  def transfer(amt : Int, acc : account) : signedTransfer = {
    if (amt > balance)
      throw new Exception("Not enough funds for transfer(" + amt + ") balance = " + balance)
    else if (0 == amt)
      throw new Exception("Attempting to transfer 0 amount")
    else {
      var t = new transfer(amt, this, acc)
      AccountSigner.setKey(PasswordHasher.hash(password.getBytes))
      var sig = t.sign(this)
      if (t.verify(acc, getPublic, sig)) {
        inProcessTransferAmount = amt // expected signed transfer processing amount
        var x = new signedTransfer(t, sig)
        balance -= amt // TODO change the balance updates to at the receive of a new block broadcast
        acc.balance += amt
        x
      } else
        null
    }
  }

  // used to update account balances after successfully processing
  // a signed transfer and adding it to the blockchain.
  def processTransfer(s : signedTransfer, seqNum : Int) = {
    var pbk = getPublic
    if (pbk.sameElements(s.from.getPublic)) { // if this is a signed transfer from this account
      // double check the signature
      if (! verify(s.t, getPublic, s.sig))
        throw new Exception("Invalid signed transfer from this account, forged!")
      else {
        balance -= s.amt
        prevTransferSeqNum = seqNum
      }
    }
    else if (pbk.sameElements(s.to.getPublic)) { // if this is a signed transfer to this account
      if (! verify(s.t, s.from.getPublic, s.sig))
        throw new Exception("Invalid signed transfer to this account, forged!")
      else {
        balance += s.amt
        prevTransferSeqNum = seqNum
      }
    }
    else {
      throw new Exception("Process transfer called on uninvolved account")
    }
  }

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

}
