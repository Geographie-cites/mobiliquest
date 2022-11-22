package mobiliquest

import mobiliquest.ThreadService.pool
import shared.data.Modality

object Utils {

   def hash(aString: String): String = {
    import java.math.BigInteger
    import java.security.MessageDigest
    val md = MessageDigest.getInstance("MD5")
    val digest: Array[Byte] = md.digest(aString.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hashedPassword = bigInt.toString(16).trim
    "%1$32s".format(hashedPassword).replace(' ', '0')
  }
}
