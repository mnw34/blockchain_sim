
trait hash { 
  def get(data : Array[Byte]) : Array[Byte] 
}

object sha256 extends hash {
  def get(data : Array[Byte]) = java.security.MessageDigest.getInstance("SHA256").digest(data)
}


