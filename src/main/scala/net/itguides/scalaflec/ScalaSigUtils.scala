package net.itguides.scalaflec

import tools.scalap.scalax.rules.scalasig._
import reflect.ScalaSignature
import scala.reflect.generic.ByteCodecs

/**
 * utility classes that interact with ScalaSigParser
 */
object ScalaSigUtils {
  /**
   * from ScalaSignature annotation values, took the ScalaSignature byte
   */
  def decodeSignatureBytes(bytes: Array[Byte]): Array[Byte] = {
    val length = ByteCodecs.decode(bytes)
    bytes.take(length)
  }
  
  /**
   * retrieve the ScalaSignature bytes from ScalaSignature annotation
   */
  def retriveScalaSignatureBytesFromAnnotation(sig: ScalaSignature): Array[Byte] = {
    decodeSignatureBytes(sig.bytes.getBytes)
  }
  
  /**
   * try to retrive ScalaSignature bytes from a class. will return an option, if 
   * ScalaSignatur annotation is not found.
   */
  def retrieveScalaSignatureBytesFromType(tpe: Class[_]): Option[Array[Byte]] = {
    tpe.getAnnotations.view collect { case x: ScalaSignature => x } map { retriveScalaSignatureBytesFromAnnotation } headOption
  }
  
  /**
   * given ScalaSignature byte array, decode it into ScalaSig
   */
  def scalaSigFromBytes(bytes: Array[Byte]): ScalaSig = {
    ScalaSigAttributeParsers.parse(ByteCode(bytes))
  }
  
  /**
   * given a type, retrieved its ScalaSignature byte and decode to ScalaSig
   */
  def scalaSigFromType(tpe: Class[_]): Option[ScalaSig] = {
    retrieveScalaSignatureBytesFromType(tpe) map (scalaSigFromBytes)
  }
  
  /**
   * retrieved a list of top level symbols from scala signature
   */
  def topLevelSymsFromScalaSig(s: ScalaSig): List[Symbol] = {
    s.topLevelClasses ++ s.topLevelObjects
  }
  
  /**
   * wrapper to print scala signature to standard out.
   */
  def decompile(s: ScalaSig): String = {
    tools.scalap.Main.parseScalaSignature(s, false)
  }
  
  /**
   * wrapper around the java relfection, to safely get a class.
   */
  def safeGetClass(name: String): Option[Class[_]] = {
    try Some(Class forName name) catch { case e: Exception => None }
  }
  
  /**
   * Given a string, will return a `Class[_] -> String` representing the longest possible
   * match that's a valid class name, paired with the remainder of the string.
   */
  def resolveClassAndRemainder(path: String) = {
    val splitPoints = path.zipWithIndex collect {case ('.',i) => i}
    val splits = (path->"") +: (splitPoints.reverse map {path.splitAt} map { case (a,b) => (a,b.tail)})
    splits.view flatMap { case (l,r) => safeGetClass(l) map (_ -> r) } headOption
  }
  
  /**
   * TODO: document this methods.
   * 
   * if not mistaken, this method will collect all the alias symbols that is not exist by the class name or inner child class name
   */
  def resolveExternal(path: String): Option[Seq[AliasSymbol]] = {
    resolveClassAndRemainder(path) map { resolv =>
      val (cls, remainder) = resolv
      val subparts = remainder split '.'
      val sig = scalaSigFromType(cls)
      val syms = sig.toSeq flatMap { _.symbols } collect {
        case sym: AliasSymbol if sym.name == remainder => sym
      }
      syms
    }
  }
  
  /**
   * found the actual type of the alias.
   */
  def deAlias(sym: AliasSymbol) = {
    sym.infoType
  }
}
