package com.etienne

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
        case clazz@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) { $self => ..$stats }" :: Nil =>
          //          val decoratedStats =
          //            stats map {
          //              case field@ValDef(mod, name@TermName("dog"), Ident(TypeName("Person")), rhs) =>
          ////                println(showRaw(mod))
          ////                println(showRaw(name))
          ////                println(showRaw(rhs))
          //                println(showRaw(field))
          //                ValDef(mod, TermName("logger"), Ident(TypeName("Person")), rhs)
          //              case other =>
          //                other
          //            }

          val newPerson: Either[c.universe.TermName, c.universe.ValDef] =
            stats.find {
              case ValDef(_, _, Ident(TypeName("Person")), _) => true
              case _ => false
            }
              .map {
                case field@ValDef(mod, name, tpt, rhs) => Left(name)
              }.getOrElse(
              Right(
                ValDef(Modifiers(),
                  TermName("logger"),
                  Ident(TypeName("Person")),
                  Apply(Select(Select(Ident(TermName("com")),
                    TermName("etienne")), TermName("Person")),
                    List(Literal(Constant("Peter")), Literal(Constant(5)))
                  )
                )
              )
            )

          val personName = newPerson match {
            case Left(name) => q"""$name"""
            case Right(ValDef(_, name, _, _)) => q"""$name"""
          }

//          println(s"here it is $personName")

          val field = newPerson match {
            case Left(_) => q""""""
            case Right(person) => person
          }

          println(newPerson)

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

}