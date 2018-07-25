package com.etienne

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class MethodLogger extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro MethodLogger.impl
}

// TODO class name of the method

object MethodLogger {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val result = {
      annottees.map(_.tree).toList match {
        case _@DefDef(mods, methodName, tpes, paramLists: scala.List[scala.List[ValDef]], returnType, body) :: Nil =>
          val params = paramLists.flatten.map(p => (p.name.encodedName.toString, p.tpt.toString, p.name))
          val paramQuote =
            if (params.nonEmpty) {
              q"""
               println("with parameters:")
               $params.foreach(x => println(x._1.toString + ": " + x._2.toString + " = " + x._3.toString))
             """
            } else {
              q"""
               println("no parameters")
             """
            }
          q"""$mods def $methodName[..$tpes](...$paramLists): $returnType =  {
            print("Method called: ")
            println(${methodName.decodedName.toString})
            $paramQuote
            $body
          }"""
        case _ => c.abort(c.enclosingPosition, "Annotation @Benchmark can be used only with methods")
      }
    }
    c.Expr[Any](result)
  }

}