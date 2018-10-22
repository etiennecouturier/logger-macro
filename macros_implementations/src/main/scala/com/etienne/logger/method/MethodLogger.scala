package com.etienne.logger.method

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
        case method@DefDef(mods, methodName, tpes, paramLists: scala.List[scala.List[ValDef]], returnType, body) :: Nil =>
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
              val newFunctionBody: Tree =
                q"""
                   println(${c.parse(requestVarName)}.host)
                  $functionBody
                  """
              Apply(stats, Function(ValDef(m, TermName("request"), tpt, rhs) :: Nil, newFunctionBody) :: Nil)
            case d => d
          }

          println(newBody)

          q"""$mods def $methodName[..$tpes](...$paramLists): $returnType =  {
            print("Method called: ")
            println(${methodName.decodedName.toString})
            $paramQuote
            $newBody
          }"""
        case _ => c.abort(c.enclosingPosition, "Annotation @Benchmark can be used only with methods")
      }
    }
    c.Expr[Any](result)
  }

}