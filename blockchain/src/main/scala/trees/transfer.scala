// transfer.scala
// represents a single transaction of an amount from one account balance to another
class transfer(var amt : Int, var from : account, var to : account) extends signable {
  var hasher = from
  def hashLen = from.hashLen
  var signer = from

  // this returns the transaction data from signing as a string of bytes
  // two input balances and two output balances signed by the transfer from
  def getData = {
    // allocate enough space for the transaction data not including the signature
    var xfer = Array[Bytes](from.getPublic.length * 2 + from.balance.length * 4)
    
    var fpbk = from.getPublic ; var tpbk = to.getPublic
    var fbal = from.balace    ; var tbal = to.balance

    // helper function to fill in the transfer data
    var i = 0
    val fill = (x : Array[Byte]) => { x.copyToArray(xfer, i) ; i += x.length }

    // from/to public, original balance, new balance
    fill(fpbk) ; fill(fbal) ; fill(fbal - amt)
    fill(tpbk) ; fill(tbal) ; fill(tbal + amt)

    xfer // return
  }
}
