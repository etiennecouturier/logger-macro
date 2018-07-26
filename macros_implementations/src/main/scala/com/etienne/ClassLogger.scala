package com.etienne

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

class ClassLogger extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ClassLogger.impl
}

object ClassLogger {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    c.Expr[Any]({
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
                     logger.debug("with parameters:" + $params.map(x => (x._1.toString + ": " + x._2.toString + " = " + x._3.toString)).toString)
                   """
                  } else {
                    q"""
                     logger.debug("no parameters")
                   """
                  }
                q"""$mo def $methodName[..$tpes](...$paramLists): $returnType =  {
                  logger.debug("Method called: " + ${tpname.toString} + "@" + ${methodName.decodedName.toString})
                  $paramQuote
                  $body
                }"""
              case field@ ValDef(_, name, q"""Logger""", _) =>
                println(name.decodedName)
                field
              case other => other
            }

          val (loggerName, loggerField) = getLogger(stats, c)
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) {
            $self =>
             $loggerField

            ..$decoratedStats
          }"""
        case _ => c.abort(c.enclosingPosition, "Annotation @ClassLogger can only be used with classes")
      }
    })
  }


  private def getLogger(stats: Seq[Trees#Tree], c: blackbox.Context): (c.universe.Tree, c.universe.Tree) = {
    import c.universe._

    val newPerson: Either[c.universe.TermName, c.universe.ValDef] =
      stats.find {
        case ValDef(_, _, Ident(TypeName("Logger")), _) => true
        case _ => false
      }
        .map {
          case field@ValDef(mod, name, tpt, rhs) => Left(name)
        }.getOrElse(
        Right(
          q"""val logger = Logger(this.getClass)""".asInstanceOf[ValDef]
        ))

    val personName = newPerson match {
      case Left(name) => q"""$name"""
      case Right(ValDef(_, name, _, _)) => q"""$name"""
    }

    val field = newPerson match {
      case Left(_) => q""""""
      case Right(person) => person
    }
    (personName, field)
  }

}