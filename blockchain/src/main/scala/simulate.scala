import scala.collection.mutable.ArrayBuffer

/* simulate.scala
 * simulates the blockchain
 */
object simulate {
  def main(args: Array[String]): Unit = {

    val NumPeers = 5
    val InitialBlockChainBufferSize = 10
    val RootAccountInitialBalance = 100
    val RootAccountPassword = "root"
    val AccountPassword = "account"
    var accountPasswordNum = 1

    val rand = scala.util.Random

    // the blockchain is simulated as an array for now
    var blockchain = new ArrayBuffer[signedTransfer](InitialBlockChainBufferSize)

    // simulated network of peers in an array for now
    var peers = new ArrayBuffer[account_manager](NumPeers)

    def getAccountManager : account_manager = {
      var am: account_manager = null
      while (null == am)
        am = peers(rand.nextInt(peers.length - 1))
      am
    }

    def createAccount : Unit = {
      var am = getAccountManager
      am.create(AccountPassword + accountPasswordNum)
      accountPasswordNum += 1
    }

    def getAccount(l : List[account]) = {
      if (null == l || 0 == l.length)
        null
      else if (1 == l.length)
        l(0)
      else
        l(rand.nextInt(l.length - 1))
    }

    // spawn peers as account manager threads
    var amID = PeerThreadID("AccountManager", 0)
    for (i <- 0 until NumPeers) {
      peers.append(new account_manager(amID))
      amID = PeerThreadID(amID.name, amID.id + 1)
    }

    // create a root account with an initial balance
    var root = peers(0).create(RootAccountPassword)
    root.balance = RootAccountInitialBalance

    // create an initial transfer to itself to start the blockchain
    root.transfer(RootAccountInitialBalance, root)

    // create a few accounts
    for (i <- 0 until 5) {
      createAccount
    }

    // start all of the threads
    peers.foreach({ (p : peer[peer_data]) => p.start })

    // run the simulation
    while (true) {

      // create a random transfer
      {
        // get two random account managers, may be the same
        var am1 = peers(rand.nextInt(peers.length - 1))
        var am1Accs = am1.getlist
        var am2Accs = peers(rand.nextInt(peers.length - 1)).getlist

        if (null != am1Accs && 0 != am1Accs.length && null != am2Accs && 0 != am2Accs.length) {
          // get two random accounts, may be the same
          var a = getAccount(am1Accs)
          var b = getAccount(am2Accs)

          // create the transfer
          if (0 != a.balance) {
            var v = rand.nextInt(a.balance)
            if (0 == v) v = 1
            if (!am1.transfer(a, v, b))
              throw new Exception("Random transfer failed " + a + " to " + b)
          }
        }
      }

      // roll a random to create an account
      if (0 == rand.nextInt(5))
        createAccount

      // make all peers send discovery data
      peers.foreach({ (am : account_manager) => am.broadcast(PeerDataFactory.getPeerData(am)) })
    }
  }


}
