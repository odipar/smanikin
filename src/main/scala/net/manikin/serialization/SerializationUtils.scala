package net.manikin.serialization

object SerializationUtils {

  import java.io.ByteArrayOutputStream

  import com.twitter.chill.{Input, Output, ScalaKryoInstantiator}
  
  val kryo = {
    val i = new ScalaKryoInstantiator()
    i.setRegistrationRequired(false)
    i.newKryo()
  }
  
  def deepCloneKryo[X](o: X): X = {
    val baos = new ByteArrayOutputStream()
    val output = new Output(baos)

    kryo.writeObject(output, o)
    output.close()

    kryo.readObject(new Input(baos.toByteArray), o.getClass)
  }
}
