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
        case cl@q"${
        mod@Modifiers(_, _, _)} class $tpname[..$tparams] $ctorMods(...$paramss) { $self => ..$stats }" :: Nil =>
          val decoratedStats =
            stats map {
              case method@DefDef(mo@Modifiers(_, _, _), methodName, tpes, paramLists: scala.List[scala.List[ValDef]], returnType, body) =>
                println(showRaw(method))
                val k = method.find(t =>
                  if (t.isTerm) {
                    println(show(t.tpe))
                    t.tpe == "Person"
                  } else {
                    false
                  }
                )
                println(k)
                //              val tn = Ident(TermName("Person").toTypeName
                //              println("-----------------" + showRaw(method) + "-----------------")
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