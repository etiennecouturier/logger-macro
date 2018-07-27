package com.etienne.logger.clazz

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class NoLogging extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro NoLogging.impl
}

object NoLogging {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val result = {
      annottees.map(_.tree).toList match {
        case _@DefDef(mods, methodName, tpes, paramLists: scala.List[scala.List[ValDef]], returnType, body) :: Nil =>
          q"""$mods def $methodName[..$tpes](...$paramLists): $returnType =  {
            $body
          }"""
        case _ => c.abort(c.enclosingPosition, "Annotation @NoLogging can be used only with methods")
      }
    }
    c.Expr[Any](result)
  }

}