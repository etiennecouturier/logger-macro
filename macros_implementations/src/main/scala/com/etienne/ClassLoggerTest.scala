package com.etienne

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

class ClassLoggerTest extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ClassLoggerTest.impl
}

object ClassLoggerTest {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    c.Expr[Any]({
      annottees.map(_.tree).toList match {
        case _@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) { $self => ..$stats }" :: Nil =>

          val (personName, field) = getLogger(stats, c)

          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) {
            $self =>
             $field
             $personName.age
            ..$stats
          }"""
        case _ => c.abort(c.enclosingPosition, "Annotation @ClassLogger can only be used with classes")
      }
    })
  }

  private def getLogger(stats: Seq[Trees#Tree], c: blackbox.Context): (c.universe.Tree, c.universe.Tree) = {
    import c.universe._

    val newPerson: Either[c.universe.TermName, c.universe.ValDef] =
      stats.find {
        case ValDef(_, _, Ident(TypeName("Person")), _) => true
        case _ => false
      }
        .map {
          case field@ValDef(mod, name, tpt, rhs) => Left(name)
        }.getOrElse(
        Right(
          q"""val logger: Person = Person("Peter", 5)""".asInstanceOf[ValDef]
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