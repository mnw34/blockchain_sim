// transfer.scala
// represents a single transaction of an amount from one account balance to another
// extends signable, so should only be signed and verified by accounts from and to respectively

import crypto.{signable, verifier}
import scala.collection.mutable.ArrayBuffer

object SignedTransferFactory {

  val pbkEncodedLength = 4
  val balEncodedLength = 8
  val prevTransferEncodedLength = 8

  def getSignedTransfer(a : Array[Byte]) = {
    var i = prevTransferEncodedLength
    var fromPrev   = a.take(i).toString.toInt
    var fromPbkLen = a.slice(i, pbkEncodedLength).toString.toInt
    i += pbkEncodedLength
    var fromPbk    = a.slice(i, fromPbkLen)
    i += fromPbkLen
    var fromOldBal = a.slice(i, balEncodedLength).toString.toInt
    i += balEncodedLength
    var fromNewBal = a.slice(i, balEncodedLength).toString.toInt
    i += balEncodedLength
    var toPrev     = a.slice(i, prevTransferEncodedLength).toString.toInt
    i += prevTransferEncodedLength
    var toPbkLen   = a.slice(i, pbkEncodedLength).toString.toInt
    i += pbkEncodedLength
    var toPbk      = a.slice(i, toPbkLen)
    i += toPbkLen
    var toOldBal   = a.slice(i, balEncodedLength).toString.toInt
    i += balEncodedLength
    var toNewBal   = a.slice(i, balEncodedLength).toString.toInt

    var sig = a.takeRight(a.length - i)

    assert(fromOldBal - fromNewBal == toOldBal + toNewBal)

    var from = new public_account(fromPbk, fromOldBal, fromPrev)
    var to   = new public_account(toPbk,   toOldBal,   toPrev)
    var t    = new transfer(fromOldBal - fromNewBal, from, to)
    var st   = new signedTransfer(t, sig)
    assert(st.getData.sameElements(a))
    st
  }
}

// signed transfer data bytes contains the transfer data and appended signature
class signedTransfer(val t : transfer, val sig : Array[Byte]) extends transfer(t.amt, t.from, t.to) {
  override val data : Array[Byte] = null
  override def getData = t.getData ++ sig
  override def setData(data : Array[Byte]) = throw new Exception("Attempting to change data of signed transfer")

  override def toString = {
    var s = ""
    def byteToHexString(x : Byte) = { s += "%02x" format x }
    getData.foreach(byteToHexString)
    s
  }
}

// getData returns the transfer structure
// it is the account holders responsibility to update its balance
// after creating and signing the transfer
// transfer data has the following structure as an array of bytes returned from getData
// from account previous transfer sequence number - 4 byte unsigned value as 4 byte ASCII hex
// from account public key length - 2 byte unsigned value as 4 byte ASCII hex
// from account public key
// from account old balance       - 4 byte Integer as 8 byte ASCII hex
// from account new balance       - 4 byte Integer as 8 byte ASCII hex
// to account previous transfer sequence number - 4 byte unsigned value as 4 byte ASCII hex
// to account public key length   - 2 byte unsigned value as 4 byte ASCII hex
// to account old balance         - 4 byte Integer as 8 byte ASCII hex
// to account new balance         - 4 byte Integer as 8 byte ASCII hex
class transfer(var amt : Int, var from : account, var to : account) extends signable {

  if (from.balance < amt)
    throw new Exception("Invalid transfer attempted [" + amt + "] from " + from + " to " + to)

  val data : Array[Byte] = {
    var fpbk = from.getPublic
    var tpbk = to.getPublic

    // allocate enough space for the transaction data
    var xfer = new ArrayBuffer[Byte]((SignedTransferFactory.prevTransferEncodedLength * 2) +
      (SignedTransferFactory.pbkEncodedLength * 2) +
      fpbk.length + tpbk.length + (4 * SignedTransferFactory.balEncodedLength))

    // helper function to fill in the transfer data
    var i = 0
    val fill = (x : Array[Byte]) => { xfer.insertAll(i, x) ; i += x.length }

    // encode an integer into len bytes
    val encodeInt = (x : Int, len : Int) => {
      var byteBuff = new ArrayBuffer[Byte](len)
      var hex = x.toHexString
      var bytes = hex.getBytes
      assert(len >= bytes.length)
      for (i <- 0 until len - bytes.length) {
        var t = 0
        byteBuff.insertAll(i, t.toHexString.getBytes)
      }
      byteBuff.insertAll(len - bytes.length, bytes)
      fill(byteBuff.toArray)
    }

    encodeInt(from.prevTransferSeqNum, SignedTransferFactory.prevTransferEncodedLength)
    encodeInt(fpbk.length, SignedTransferFactory.pbkEncodedLength)
    fill(fpbk)
    encodeInt(from.balance, SignedTransferFactory.balEncodedLength)
    encodeInt(from.balance - amt, SignedTransferFactory.balEncodedLength)

    encodeInt(to.prevTransferSeqNum, SignedTransferFactory.prevTransferEncodedLength)
    encodeInt(tpbk.length, SignedTransferFactory.pbkEncodedLength)
    fill(tpbk)
    encodeInt(to.balance, SignedTransferFactory.balEncodedLength)
    encodeInt(to.balance + amt, SignedTransferFactory.balEncodedLength)

    xfer.toArray
  }

  // hashable implementations
  // this returns the transaction data for signing as a string of bytes
  // two input balances and two output balances from and to accounts
  // must be signed by the from account and verified by the to account
  def getData = data
  def setData(data : Array[Byte]) : Unit = throw new Exception("Cannot set data of transaction")
}
