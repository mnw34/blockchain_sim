// peer.scala

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ArrayBuffer

case class NetAddr(ip : String, port : Int)

// provide an initial size for the internal peer buffer
// provide the number of peers to forward to all other peers during a broadcast
// provide the delay in seconds between broadcasts
case class PeerConfig(initPeerBufferSize : Int,
                      numPeersToForward  : Int,
                      secondsDelayToBroadcast : Int)

class spinLock extends AtomicInteger(0) {
  def locked = 0 == get
  def lock = while (compareAndSet(0, 1)) { }
  def unlock = while (compareAndSet(0, 1)) { }
}

abstract class peer[T](var netAddr : NetAddr, var peerConfig : PeerConfig)
  extends Thread {

  def peerStart = run
  def peerStop  = threadLock.unlock
  def threadLock = new spinLock

  // interface definition
  def send(t : T) : Unit                  // send data to the peer
  def check : ArrayBuffer[T]              // check the server input for data, may return null
  def process(a : ArrayBuffer[T]) : Unit  // process received data, may be null input

  // run the peer as a server
  override def run = {
    threadLock.lock
    var lastTime = System.currentTimeMillis
    while (threadLock.locked) {

      // check for broadcast data
      if (bcastDataPresent.locked) {
        // check time for broadcast
        if (System.currentTimeMillis - lastTime  > peerConfig.secondsDelayToBroadcast) {
          bcastDataLock.lock
          peersLock.lock
          peers.foreach({ (p : peer[T]) => if (null != p) p.send(bcastData(0)) })
          peersLock.unlock
          bcastDataLock.unlock
          bcastDataPresent.unlock
          lastTime = System.currentTimeMillis
        }
      }

      // check listening socket and process data
      process(check)
    }
  }

  protected var bcastData        = new ArrayBuffer[T](1)
  protected var bcastDataLock    = new spinLock
  protected var bcastDataPresent = new spinLock

  // broadcast the generic type to all peers
  def broadcast(t : T) : Unit = {
    if (!threadLock.locked) throw new Exception("Peer not running, cannot broadcast")
    bcastDataLock.lock
    bcastData = ArrayBuffer[T](t)
    bcastDataLock.unlock
    bcastDataPresent.lock
  }

  var peers : Array[peer[T]] = new Array[peer[T]](1)
  var peerForwardIdx = 0
  var peersLock = new spinLock
  var numPeers = 0

  def == = (p : peer[T]) => (netAddr == p.netAddr)

  def add(p : peer[T]) = {
    peersLock.lock
    peers = p +: peers
    numPeers += 1
    peersLock.unlock
  }

  // returns index of peer in peer array or -1 if not found
  def find(p : peer[T]) = {
    peersLock.lock
    val f = (_p : peer[T]) => p == _p
    var t = peers.indexWhere(f)
    peersLock.unlock
    t
  }

  def remove(p : peer[T]) = {
    var t = find(p)
    if (0 <= t) {
      peersLock.lock
      var u = peers.dropRight(peers.length - t)
      var v = peers.drop(t + 1)
      peers = u ++ v
      peersLock.unlock
    }
  }
}
