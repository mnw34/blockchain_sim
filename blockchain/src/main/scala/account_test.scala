object account_test {
  def main(args : Array[String]) {
    var pw = "1"

    var rand = scala.util.Random

    // create an array of accounts
    var l = new Array[account](args(0).toInt)
    for (i <- 0 until l.length) {
      l(i) = new account(pw.toString)
      l(i).balance = 10
      println("new account - " + l(i))
      var pwH = PasswordHasher.hash(pw.getBytes)
      var xp = AccountKeyPairGenerator.getPublic(pwH)
      var p = l(i).getPublic
      assert(xp.sameElements(p))
      pw = (pw.toInt + 1).toString
    }

    println
    println("random transfers")
    for (i <- 0 until l.length * 2) {
      var a = l(rand.nextInt(l.length))
      var b = l(rand.nextInt(l.length))
      var v = rand.nextInt(a.balance + 1)

      if (a != b && 0 != v) {

        println("transfer [$" + v + "] from " + a + " to " + b)
        var st = a.transfer(v, b)
        println("complete [$" + v + "] from " + a + " to " + b)
        println("signed transfer " + st)
      }
    }

    println("PASSED")
  }
}
