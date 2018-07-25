package com.etienne

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class ClassLogger extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ClassLogger.impl
}

object ClassLogger {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val result = {
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) { $self => ..$stats }" :: Nil =>
          val decoratedStats =
            stats map {
              case _@DefDef(mo, methodName, tpes, paramLists: scala.List[scala.List[ValDef]], returnType, body) =>
                val params = paramLists.flatten.map { p =>
                  (p.name.encodedName.toString, p.tpt.toString, p.name)
                }

                val paramQuote =
                  if (params.nonEmpty) {
                    q"""
                     logger.debug("with parameters:")
                     $params.foreach(x => logger.debug(x._1.toString + ": " + x._2.toString + " = " + x._3.toString))
                   """
                  } else {
                    q"""
                     logger.debug("no parameters")
                   """
                  }
                q"""$mo def $methodName[..$tpes](...$paramLists): $returnType =  {
                  logger.debug("Method called: ")
                  logger.debug(${tpname.toString} + "@")
                  logger.debug(${methodName.decodedName.toString})
                  $paramQuote
                  $body
                }"""
              case other => other
            }

          val finalStats =
            decoratedStats :+
              q"""
                 import play.api.Logger
                 protected val logger = Logger(this.getClass)
              """

          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) {
            $self => ..$finalStats
          }"""
        case _ => c.abort(c.enclosingPosition, "Annotation @TalkingAnimal can be used only with case classes which extends Animal trait")
      }
    }
    c.Expr[Any](result)
  }

}