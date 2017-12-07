// account_manager.scala

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ArrayBuffer
import crypto.sha256

object AccountManagerPeerConfig {
  val InitPeerBufferSize      = 50
  val NumPeersToForward       = 5
  val SecondsDelayToBroadcast = 10
  val MaxPeerThreadData       = 20
  val Config = PeerThreadConfig(MaxPeerThreadData,
    PeerConfig(InitPeerBufferSize, NumPeersToForward, SecondsDelayToBroadcast))
}

object PeerDataTypes {
  val PEER_DATA            = 0
  val ACCOUNT_REQUEST_DATA = 1
  val ACCOUNT_DATA         = 2
  val SIGNED_TRANSFER_DATA = 3
}

class peer_data(val pdt : Int = PeerDataTypes.PEER_DATA, val threadID : PeerThreadID)

class account_request_data(threadID : PeerThreadID, val pubKey : Array[Byte])
  extends peer_data(PeerDataTypes.ACCOUNT_REQUEST_DATA, threadID)

class account_data(threadID : PeerThreadID, val pubKey : Array[Byte], val balance : Int, val prevTransferSeqNum : Int)
  extends peer_data(PeerDataTypes.ACCOUNT_DATA, threadID)

class signed_transfer_data(threadID : PeerThreadID, val signedTransferData : Array[Byte])
  extends peer_data(PeerDataTypes.SIGNED_TRANSFER_DATA, threadID)

object PeerDataFactory {

  def getPeerData(p : peer[peer_data]) = new peer_data(threadID = p.asInstanceOf[account_manager].threadID)

  def getAccountRequestData(p : peer[peer_data], a : account) =
    new account_request_data(p.asInstanceOf[account_manager].threadID, a.getPublic)

  def getAccountData(p : peer[peer_data], a : account) =
    new account_data(p.asInstanceOf[account_manager].threadID, a.getPublic, a.balance, a.prevTransferSeqNum)

  def getSignedTransferData(p : peer[peer_data], t : signedTransfer) =
    new signed_transfer_data(p.asInstanceOf[account_manager].threadID, t.getData)
}

// the account manager names must be unique
class account_manager(val threadID : PeerThreadID)
  extends peer_thread[peer_data](threadID, AccountManagerPeerConfig.Config) {

  var accounts : rbt[account] = null
  var blocks   : Array[block]   = null

  var expectedAccountData = new AtomicInteger(0)

  def process(a : ArrayBuffer[peer_data]) : Unit = { // process received data, may be null input
    if (null != a) {
      a.foreach(processPeerData)
    }
  }

  protected def processPeerData(pd : peer_data) = {
    if (pd.pdt == PeerDataTypes.PEER_DATA) {
      // check peer list
      var t = new account_manager(pd.threadID)
      if (0 > find(t)) { // not found
        add(t)           // add the new peer
      }
      broadcast(pd)    // broadcast peer to all peers
    }
    else if (pd.pdt == PeerDataTypes.ACCOUNT_REQUEST_DATA) {
      // check accounts
      var a = find(pd.asInstanceOf[account_request_data].pubKey)
      if (null != a) { // found
        // respond to peer with public account data
        send(new account_data(threadID, a.getPublic, a.balance, a.prevTransferSeqNum))
      }
      else {
        broadcast(pd) // broadcast account request to all peers
      }
    }
    else if (pd.pdt == PeerDataTypes.ACCOUNT_DATA) {
      // verify this peer is expecting the account data peer message
      var t = expectedAccountData.get
      if (0 < t) {
        // add the public account data to the managed accounts
        expectedAccountData.getAndDecrement
        var d = pd.asInstanceOf[account_data]
        var a = new public_account(d.pubKey, d.balance, d.prevTransferSeqNum)
        accounts.insert(a)
      }
      else {
        throw new Exception("Peer received unrequested account data")
      }
    }
    else if (pd.pdt == PeerDataTypes.SIGNED_TRANSFER_DATA) {
      // TODO forward to all miner accounts,
      // for now just accept the transfer and add to the blockchain
      var st = pd.asInstanceOf[signed_transfer_data]
      addBlock(SignedTransferFactory.getSignedTransfer(st.signedTransferData))
    }
    else {
      throw new Exception("Invalid peer data type")
    }
  }

  def addBlock(st : signedTransfer) = {
    if (null == blocks) { // create root block
      blocks = new Array[block](1)
      var b = new rootBlock(sha256, st)
      blocks(0) = b
    }
    var t = new Array[block](1)
    t(0) = new block(blocks.last, sha256, st)
    blocks = blocks ++ t
  }

  def create(password : String) = {
    var a = new rbt[account](new account(password), accSearchLessThan, accSearchEquals)
    if (null == accounts)
      accounts = a
    else
      accounts = accounts.insert(a)

    // broadcast the new account to all known peers
    broadcast(PeerDataFactory.getAccountData(this, a.value))

    a.value // return the new account object
  }

  // returns true if the signed transfer was successfully created and broadcasted
  def transfer(from : account, amt : Int, to : account) : Boolean = {
    var signedTransfer = from.transfer(amt, to)
    if (null == signedTransfer)
      false
    else {
      broadcast(PeerDataFactory.getSignedTransferData(this, signedTransfer))
      true
    }
  }

  // returns null if not found, but broadcasts the public account request
  // and sets the flag for expected account data
  def find(pubKey : Array[Byte]) : account = {
    accountSearchNode.pubKey = pubKey
    var t = accounts.find(accountSearchNode)
    if (null == t) { // send a request for the account
      broadcast(PeerDataFactory.getAccountRequestData(this, accountSearchNode))
      expectedAccountData.getAndIncrement
      null
    }
    else
      t.value
  }

  def getlist = if (null != accounts) accounts.asList else null

  // less than function for searching managed accounts
  val accSearchLessThan = (x : account, y : account) => {
    var xpbk = x.getPublic
    var i = 0
    var less = false
    var check = (u : Byte) => {
      if (i < xpbk.length && xpbk(i) >= u)
        i += 1
      else
        less = true
    }
    y.getPublic.foreach(check)
    less
  }

  val accSearchEquals = (x : account, y : account) =>
    x.getPublic.sameElements(y.getPublic)
}

object accountSearchNode extends account("password") {
  var pubKey : Array[Byte] = null
  override def getPublic = pubKey
}
