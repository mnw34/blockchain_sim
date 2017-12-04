// account.scala

import crypto.keypair
import crypto.signer
import crypto.verifier

// TODO update to use real signature algorithms
import crypto.dumbKeyPair
import crypto.DumbSigner
import crypto.DumbVerifier

// an account has an asymmetric key pair, and a balance
class account(val password : String) extends keypair(password.getBytes()) with signer with verifier {
  var balance = 0

  // TODO update to use real signature algorithms
  var keypair  = new dumbKeyPair(privKey)
  var signer   = DumbSigner
  var verifier = DumbVerifier

  def transfer(amt : Int, acc : account) = {
    if (amt > balance) 
      throw new Exception("Not enough funds for transfer(" + amt + ") balance = " + balance)
    else {
      
    }
  }
}
