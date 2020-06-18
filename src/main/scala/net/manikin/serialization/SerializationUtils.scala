package net.manikin.serialization

import com.twitter.chill.KryoBase

object SerializationUtils {

  import java.io.ByteArrayOutputStream

  import com.twitter.chill.{Input, Output, ScalaKryoInstantiator}
  
  
  def deepClone[X](o: X, kryo: KryoBase): X = {
    val buffer = new Array[Byte](16384)
    val output = new Output(buffer)

    kryo.writeClassAndObject(output, o)
    output.close()

    kryo.readClassAndObject(new Input(output.toBytes)).asInstanceOf[X]
  }

  def toBytes[X](o: X, buffer: Array[Byte] = new Array[Byte](16384), kryo: KryoBase): Array[Byte] = {
    val output = new Output(buffer)

    kryo.writeClassAndObject(output, o)
    output.close()

    output.toBytes
  }

  def toObject[X](b: Array[Byte], kryo: KryoBase): X = kryo.readClassAndObject(new Input(b)).asInstanceOf[X]
}
