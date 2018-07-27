package com.etienne.logger.clazz

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class ClassLoggerTest extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ClassLoggerTest.impl
}

object ClassLoggerTest {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    c.Expr[Any]({
      annottees.map(_.tree).toList match {
        case _@q"${mod@Modifiers(_, _, _)} class $tpname[..$tparams] $ctorMods(...$paramss) { $self => ..$stats }" :: Nil =>
          val decoratedStats =
          stats map {
            case _@DefDef(mo@Modifiers(_, _, annotations), methodName, tpes, paramLists: scala.List[scala.List[ValDef]], returnType, body)
              if !annotations.exists(_.equalsStructure(q"""new NoLogging"""))
            =>
              println(showRaw(annotations))
              q"""$mo def $methodName[..$tpes](...$paramLists): $returnType =  {
                println("hello!!!")
                $body
              }"""
            case other =>
              println("other stuff")
              other
          }
          q"""$mod class $tpname[..$tparams] $ctorMods(...$paramss) {
            $self =>
            ..$decoratedStats
          }"""
        case _ => c.abort(c.enclosingPosition, "Annotation @ClassLoggerTest can only be used with classes")
      }
    })
  }
}