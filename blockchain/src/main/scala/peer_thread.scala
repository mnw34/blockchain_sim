// peer_thread.scala
// implements a peer as thread peers instead of network peers

import scala.collection.mutable.ArrayBuffer

case class PeerThreadConfig(maxThreadData : Int, peerConfig : PeerConfig)
case class PeerThreadID(name : String, id : Int)
object PeerThreadDummyNetAddr extends NetAddr("thread", 0)

abstract class peer_thread[T](var peerThreadID : PeerThreadID, peerThreadConfig : PeerThreadConfig)
  extends peer[T](PeerThreadDummyNetAddr, peerThreadConfig.peerConfig) {

  var data : ArrayBuffer[T] = new ArrayBuffer[T](peerThreadConfig.maxThreadData)
  var dataLock = new spinLock
  var dataIdx = 0

  override def == = (p : peer[T]) => (peerThreadID == p.asInstanceOf[peer_thread[T]].peerThreadID)

  // interface definition
  def send(t : T) : Unit = { // send data to the peer
    var sent = false
    while (! sent) {
      dataLock.lock
      if (peerThreadConfig.maxThreadData == dataIdx) {
        dataLock.unlock
      }
      else {
        data(dataIdx) = t
        dataIdx += 1
        dataLock.unlock
        sent = true
      }
    }
  }

  def check : ArrayBuffer[T] = { // check the server input for data, may return null
    var t : ArrayBuffer[T] = null
    dataLock.lock
    if (0 != dataIdx) {
      t = new ArrayBuffer[T](peerThreadConfig.maxThreadData - dataIdx)
      data.copyToBuffer(t)
      dataIdx = 0
    }
    dataLock.unlock
    t
  }

}
