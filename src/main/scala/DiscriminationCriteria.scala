import scala.compiletime.summonInline
import scala.quoted.{Expr, Quotes, Type}

trait DiscriminationCriteria[-S] {
  transparent inline def discriminator[P <: S]: Int
}

inline def summonDiscriminator[SumType, VariantType <: SumType]: Int =
  summonInline[DiscriminationCriteria[SumType]].discriminator[VariantType]

transparent inline def someMacro[SumType, VariantType]: Unit = ${ someMacroImpl[SumType, VariantType] }

def someMacroImpl[SumType: Type, VariantType: Type](using quotes: Quotes): Expr[Unit] = {
  val discriminatorExpr: Expr[Int] = Expr.summon[DiscriminationCriteria[SumType]] match {
    case Some(_) =>
      '{ summonDiscriminator[SumType, VariantType & SumType] }
    case None =>
      // Fall back to the alphanumerical index
      Expr[Int](??? /*alphanumericIndex*/)
  }

  '{ () }
}