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
    val baos = new ByteArrayOutputStream()
    val output = new Output(baos)

    kryo.writeClassAndObject(output, o)
    output.close()

    kryo.readClassAndObject(new Input(baos.toByteArray)).asInstanceOf[X]
  }

  def toBytes[X](o: X, baos: ByteArrayOutputStream = new ByteArrayOutputStream(256)): Array[Byte] = {
    val output = new Output(baos)

    kryo.writeClassAndObject(output, o)
    output.close()

    baos.toByteArray
  }

  def toObject[X](b: Array[Byte]): X = {
    kryo.readClassAndObject(new Input(b)).asInstanceOf[X]
  }

}
