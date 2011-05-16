package net.itguides.scalaflec

import java.{lang => jl}
import java.lang.reflect.{ Type => JType, Array => _, _ }
import scala.reflect.Manifest
import scala.reflect.Manifest.{ classType, intersectionType, arrayType, wildcardType }

/**
 * Manifest Util, will try to get the Manifest of any class, including a java class that is mirrored (all the default types)
 */
object ScalaJavaTypeBridge { 
  val ByteClass = classOf[scala.Byte]
  val ShortClass = classOf[scala.Short]
  val CharClass = classOf[scala.Char]
  val IntClass = classOf[scala.Int]
  val LongClass = classOf[scala.Long]
  val FloatClass = classOf[scala.Float]
  val DoubleClass = classOf[scala.Double]
  val BooleanClass = classOf[scala.Boolean]
  val NullClass = classOf[scala.Null]
  val UnitClass = classOf[scala.Unit]

  val JByteClass = classOf[jl.Byte]
  val JShortClass = classOf[jl.Short]
  val JCharClass = classOf[jl.Character]
  val JIntClass = classOf[jl.Integer]
  val JLongClass = classOf[jl.Long]
  val JFloatClass = classOf[jl.Float]
  val JDoubleClass = classOf[jl.Double]
  val JBooleanClass = classOf[jl.Boolean]
  
  /**
   * simulating the Manifet.classType(x), with the bride between java and scala
   */
  def manifestOf(c : Class[_]) = c match {
    case JByteClass    | jl.Byte.TYPE      | ByteClass    => Manifest.Byte
    case JShortClass   | jl.Short.TYPE     | ShortClass   => Manifest.Short
    case JCharClass    | jl.Character.TYPE | CharClass    => Manifest.Char
    case JIntClass     | jl.Integer.TYPE   | IntClass     => Manifest.Int
    case JLongClass    | jl.Long.TYPE      | LongClass    => Manifest.Long
    case JFloatClass   | jl.Float.TYPE     | FloatClass   => Manifest.Float
    case JDoubleClass  | jl.Double.TYPE    | DoubleClass  => Manifest.Double
    case JBooleanClass | jl.Boolean.TYPE   | BooleanClass => Manifest.Boolean
    case                 jl.Void.TYPE      | UnitClass    => Manifest.Unit
    case                 null              | NullClass    => Manifest.Null
    case x => classType(x)
  }
  
  /**
   * derived an intersect type given java type.
   */
  def intersect(tps: JType*): Manifest[_] = {
    intersectionType(tps map javaType: _*)
  }
  
  /**
   * derived a manivest from a java type
   */
  def javaType(tp: JType): Manifest[_] = tp match {
    case null => Manifest.Null
    case x: Class[_]            => manifestOf(x)
    case x: ParameterizedType   =>
      val owner = x.getOwnerType
      val raw   = x.getRawType() match { case clazz: Class[_] => clazz }
      val targs = x.getActualTypeArguments() map javaType

      (owner == null, targs.isEmpty) match {
        case (true, true)   => javaType(raw)
        case (true, false)  => classType(raw, targs.head, targs.tail: _*)
        case (false, _)     => classType(javaType(owner), raw, targs: _*)
      }
    case x: GenericArrayType    => arrayType(javaType(x.getGenericComponentType))
    case x: WildcardType        => wildcardType(intersect(x.getLowerBounds: _*), intersect(x.getUpperBounds: _*))
    case x: TypeVariable[_]     => intersect(x.getBounds(): _*)
  } 
}