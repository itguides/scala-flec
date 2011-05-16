package net.itguides.scalaflec

import java.util.regex.Pattern
import tools.scalap.scalax.rules.scalasig._

/**
 * naming converter from source code to java byte code.
 */
object ScalaNaming {
    /**
     * all constructor symbols name will be <init>
     */
	val constructorName = "<init>"
	
	/**
	 * all refinement class symbols name will be <refinement>
	 */
	val refinementClassName = "<refinement>"
	
	/**
	 * all symbols in scala source code will be changed to java byte code using the following convention
	 */
	val syms = Map("\\$bar" -> "|",
				   "\\$tilde" -> "~",
				   "\\$bang" -> "!",
				   "\\$up" -> "^",
				   "\\$plus" -> "+",
				   "\\$minus" -> "-",
				   "\\$eq" -> "=",
				   "\\$less" -> "<",
				   "\\$times" -> "*",
				   "\\$div" -> "/",
				   "\\$bslash" -> "\\\\",
				   "\\$greater" -> ">",
				   "\\$qmark" -> "?",
				   "\\$percent" -> "%",
				   "\\$amp" -> "&",
				   "\\$colon" -> ":",
				   "\\$u2192" -> "?",
				   "\\$hash" -> "#")
	
    /**
     * storing the reversal of the symbols. might be useful in writing a readable relective code.
     */
    val symsReverse = syms.map{_.swap}
	
	/**
	 * convert all the key into reg-ex patter, for pattern matching and replacement
	 */
	val pattern = Pattern.compile(syms.keys.mkString("", "|", ""))
	
	/**
	 * place holder patter, for method args for e.g.
	 */
    val placeholderPattern = "_\\$(\\d)+"
	
	def isConstructor(c: Symbol) = {
      c.isInstanceOf[MethodSymbol] && c.name == constructorName
	}
	
	def isRefinementClass(c: ClassSymbol) = {
	  c.name == refinementClassName
	}
	
	def decodeScalaNaming(name: String) = {
      val stripped = stripPrivatePrefix(name)
      val m = pattern.matcher(stripped)
      var temp = stripped
      while (m.find) {
        val key = m.group
        val re = "\\" + key
        temp = temp.replaceAll(re, syms(re))
      }
      
      val result = temp.replaceAll(placeholderPattern, "_")
      scala.reflect.NameTransformer.decode(result)
    }
	
	private def stripPrivatePrefix(name: String) = {
	  val i = name.lastIndexOf("$$")
      if (i > 0) name.substring(i + 2) else name
    }
}