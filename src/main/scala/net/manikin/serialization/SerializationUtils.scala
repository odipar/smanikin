package net.manikin.serialization

object SerializationUtils {

  import java.io.ByteArrayOutputStream

  import com.twitter.chill.{Input, Output, ScalaKryoInstantiator}
  
  val kryo = {
    val i = new ScalaKryoInstantiator()
    i.setRegistrationRequired(false)
    i.newKryo()
  }
  
  def deepClone[X](o: X): X = {
    val buffer = new Array[Byte](16384)
    val output = new Output(buffer)

    kryo.writeClassAndObject(output, o)
    output.close()

    kryo.readClassAndObject(new Input(output.toBytes)).asInstanceOf[X]
  }

  def toBytes[X](o: X, buffer: Array[Byte] = new Array[Byte](16384)): Array[Byte] = {
    val output = new Output(buffer)

    kryo.writeClassAndObject(output, o)
    output.close()

    output.toBytes
  }

  def toObject[X](b: Array[Byte]): X = kryo.readClassAndObject(new Input(b)).asInstanceOf[X]
}
