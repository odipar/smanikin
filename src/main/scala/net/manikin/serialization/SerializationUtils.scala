package net.manikin.serialization

object SerializationUtils {
  import java.io.ByteArrayOutputStream
  import com.twitter.chill.{Input, Output, ScalaKryoInstantiator}
  import java.security.MessageDigest
  import com.twitter.chill.KryoBase

  val kryoInstantiator = { val ist = new ScalaKryoInstantiator() ; ist.setRegistrationRequired(false) ; ist }

  def deepClone[X](o: X, kryo: KryoBase = kryoInstantiator.newKryo()): X = {
    val buffer = new Array[Byte](16384)
    val output = new Output(buffer)

    kryo.writeClassAndObject(output, o)
    output.close()

    kryo.readClassAndObject(new Input(output.toBytes)).asInstanceOf[X]
  }

  def digest[X](o: X, buffer: Array[Byte] = new Array[Byte](16384),
                kryo: KryoBase = kryoInstantiator.newKryo(),
                md: MessageDigest = MessageDigest.getInstance("SHA-256")): Array[Byte] = {
    md.update(toBytes(o, buffer, kryo))
    md.digest()
  }

  def toBytes[X](o: X, buffer: Array[Byte] = new Array[Byte](16384), kryo: KryoBase = kryoInstantiator.newKryo()): Array[Byte] = {
    val output = new Output(buffer)

    kryo.writeClassAndObject(output, o)
    output.close()

    output.toBytes
  }

  def toObject[X](b: Array[Byte], kryo: KryoBase= kryoInstantiator.newKryo()): X = {
    kryo.readClassAndObject(new Input(b)).asInstanceOf[X]
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    val tm = ((t1 - t0) * 100).toDouble / 100000.toDouble
    println("Elapsed time: " + tm + "s")
    result
  }
}