// transfer.scala
// represents a single transaction of an amount from one account balance to another
// extends signable, so should only be signed and verified by accounts from and to respectively

import crypto.{signable, verifier}
import scala.collection.mutable.ArrayBuffer

class signedTransfer(t : transfer, sig : Array[Byte]) extends signable {
  def getData = t.getData ++ sig
  def setData(data : Array[Byte]) = throw new Exception("Attempting to change data of signed transfer")

  override def toString = " " // TODO getData
}

class transfer(var amt : Int, var from : account, var to : account) extends signable {

  if (from.balance < amt)
    throw new Exception("Invalid transfer attempted [" + amt + "] from " + from + " to " + to)

  val fold = from.balance
  val told = to.balance

  val data : Array[Byte] = {
    var fbalHex = fold.toHexString

    // allocate enough space for the transaction data not including the signature
    var xfer = new ArrayBuffer[Byte](from.getPublic.length * 2 + fbalHex.length * 4)

    from.balance -= amt                           ; to.balance += amt
    var fnew = from.balance.toHexString.getBytes  ; var tnew = to.balance.toHexString.getBytes

    // helper function to fill in the transfer data
    var i = 0
    val fill = (x : Array[Byte]) => { xfer.insertAll(i, x) ; i += x.length }

    // from/to public, original balance, new balance
    var fpbk = from.getPublic
    fill(fpbk)           ; fill(fold.toHexString.getBytes) ; fill(fnew)
    fill(to.getPublic)   ; fill(told.toHexString.getBytes) ; fill(tnew)

    xfer.toArray // return
  }

  // hashable implementations
  // this returns the transaction data for signing as a string of bytes
  // two input balances and two output balances from and to accounts
  // must be signed by the from account and verified by the to account
  def getData = data
  def setData(data : Array[Byte]) : Unit = throw new Exception("Cannot set data of transaction")

  // signable implementations
  override def verify(verifier : verifier, pubKey : Array[Byte], sig : Array[Byte]) = {
    if (fold - amt != from.balance || told + amt != to.balance)
      false
    else
      super.verify(verifier, pubKey, sig)
  }
}
