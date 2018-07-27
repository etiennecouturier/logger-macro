package com.etienne.logger.clazz

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

class ClassLogger extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ClassLogger.impl
}

// TODO check all possibilities are covered + factor out common part

object ClassLogger {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    c.Expr[Any]({
      annottees.map(_.tree).toList match {
        case q"$mods class $className[..$typeParams] $ctorMods(...$classParams) { $self => ..$classBody }" :: Nil =>
          val (loggerName, loggerField) = getLogger(classBody, c)

          val decoratedBody =
            classBody map {
              case _@DefDef(mo@Modifiers(_, _, annotations), methodName, types, methodParams: scala.List[scala.List[ValDef]], returnType, methodBody)
                if !annotations.exists(_.equalsStructure(q"""new NoLogging""")) =>
                val params = methodParams.flatten.map { p =>
                  (p.name.encodedName.toString, p.tpt.toString, p.name)
                }

                val paramQuote =
                  if (params.nonEmpty) {
                    q"""
                     $loggerName.debug("with parameters:" + $params.map(x => (x._1.toString + ": " + x._2.toString + " = " + x._3.toString)).toString)
                   """
                  } else {
                    q"""
                     $loggerName.debug("no parameters")
                   """
                  }
                q"""$mo def $methodName[..$types](...$methodParams): $returnType =  {
                  $loggerName.debug("Method called: " + ${className.toString} + "@" + ${methodName.decodedName.toString})
                  $paramQuote
                  $methodBody
                }"""
              case notDef => notDef
            }

          q"""$mods class $className[..$typeParams] $ctorMods(...$classParams) {
            $self =>
             import play.api.Logger
              $loggerField
             ..$decoratedBody
          }"""
        case q"case class $className[..$typeParams] $ctorMods(...$classParams) { $self => ..$classBody }" :: Nil =>
          val (loggerName, loggerField) = getLogger(classBody, c)

          val decoratedBody =
            classBody map {
              case _@DefDef(mo@Modifiers(_, _, annotations), methodName, types, methodParams: scala.List[scala.List[ValDef]], returnType, methodBody)
                if !annotations.exists(_.equalsStructure(q"""new NoLogging""")) =>
                val params = methodParams.flatten.map { p =>
                  (p.name.encodedName.toString, p.tpt.toString, p.name)
                }

                val paramQuote =
                  if (params.nonEmpty) {
                    q"""
                     $loggerName.debug("with parameters:" + $params.map(x => (x._1.toString + ": " + x._2.toString + " = " + x._3.toString)).toString)
                   """
                  } else {
                    q"""
                     $loggerName.debug("no parameters")
                   """
                  }
                q"""$mo def $methodName[..$types](...$methodParams): $returnType =  {
                  $loggerName.debug("Method called: " + ${className.toString} + "@" + ${methodName.decodedName.toString})
                  $paramQuote
                  $methodBody
                }"""
              case notDef => notDef
            }

          q"""case class $className[..$typeParams] $ctorMods(...$classParams) {
            $self =>
             import play.api.Logger
              $loggerField
             ..$decoratedBody
          }"""
        case q"object $objectName { $self => ..$objectBody }" :: Nil =>
          val (loggerName, loggerField) = getLogger(objectBody, c)

          val decoratedBody =
            objectBody map {
              case _@DefDef(mo@Modifiers(_, _, annotations), methodName, types, methodParams: scala.List[scala.List[ValDef]], returnType, methodBody)
                if !annotations.exists(_.equalsStructure(q"""new NoLogging""")) =>
                val params = methodParams.flatten.map { p =>
                  (p.name.encodedName.toString, p.tpt.toString, p.name)
                }

                val paramQuote =
                  if (params.nonEmpty) {
                    q"""
                     $loggerName.debug("with parameters:" + $params.map(x => (x._1.toString + ": " + x._2.toString + " = " + x._3.toString)).toString)
                   """
                  } else {
                    q"""
                     $loggerName.debug("no parameters")
                   """
                  }
                q"""$mo def $methodName[..$types](...$methodParams): $returnType =  {
                  $loggerName.debug("Method called: " + ${objectName.toString} + "@" + ${methodName.decodedName.toString})
                  $paramQuote
                  $methodBody
                }"""
              case notDef => notDef
            }

          q"""object $objectName {
            $self =>
             import play.api.Logger
              $loggerField
             ..$decoratedBody
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
          case _@ValDef(_, name, _, _) => Left(name)
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