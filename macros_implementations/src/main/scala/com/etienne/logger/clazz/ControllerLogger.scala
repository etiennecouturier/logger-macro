package com.etienne.logger.clazz

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class ControllerLogger extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ControllerLogger.impl
}

// TODO check all possibilities are covered + factor out common part

object ControllerLogger {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    c.Expr[Any]({
      annottees.map(_.tree).toList match {
        case q"$mods class $className[..$typeParams] $ctorMods(...$classParams) extends Controller { $self => ..$classBody }" :: Nil =>
          val decoratedBody =
            classBody map {
              case method@DefDef(methodMods, methodName, tpes, paramLists: scala.List[scala.List[ValDef]], returnType, body) =>
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

                val newBody = body match {
                  case Apply(stats, Function(ValDef(m, TermName(requestVarName), tpt, rhs) :: Nil, functionBody) :: Nil) =>
                    val request = c.parse(requestVarName)
                    val newFunctionBody: Tree =
                      q"""
                   println($request.remoteAddress)
                   println($request.headers.headers)
                   println($request.queryString)
                   println($request.body)
                  $functionBody
                  """
                    Apply(stats, Function(ValDef(m, TermName(requestVarName), tpt, rhs) :: Nil, newFunctionBody) :: Nil)
                  case d => d
                }

                println(newBody)

                q"""$mods def $methodName[..$tpes](...$paramLists): $returnType =  {
            print("Method called: ")
            println(${methodName.decodedName.toString})
            $paramQuote
            $newBody
          }"""
              case notDef => notDef
            }

          q"""$mods class $className[..$typeParams] $ctorMods(...$classParams) extends Controller {
            $self =>
             ..$decoratedBody
          }"""
        case _ => c.abort(c.enclosingPosition, "Annotation @ClassLogger can only be used with classes")
      }
    })
  }

}