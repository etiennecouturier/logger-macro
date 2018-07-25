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
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) { $self => ..$stats }" :: Nil => {
          val decoratedStats =
            stats map {
              case _@DefDef(mo, methodName, tpes, paramLists: scala.List[scala.List[ValDef]], returnType, body) =>
                val params = paramLists.flatten.map {p =>
                  (p.name.encodedName.toString, p.tpt.toString, p.name)
                }

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
                q"""$mo def $methodName[..$tpes](...$paramLists): $returnType =  {
                  print("Method called: ")
                  print(${tpname.toString} + "@")
                  println(${methodName.decodedName.toString})
                  $paramQuote
                  $body
                }"""
              case other =>
                println("other")
                other
            }

          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) {
            $self => ..$decoratedStats
          }"""
        }
        case _ => c.abort(c.enclosingPosition, "Annotation @TalkingAnimal can be used only with case classes which extends Animal trait")
      }
    }
    c.Expr[Any](result)
  }

}