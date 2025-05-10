import scala.quoted.{Expr, Quotes, Type}

trait DiscriminationCriteria[-S] {
  transparent inline def discriminator[P <: S]: Int
}

trait DiscriminationCriteria1[-S] extends DiscriminationCriteria[S] {
  transparent inline def discriminator[P <: S]: Int = ???
}

transparent inline def someMacro[SumType, VariantType]: Unit = ${ someMacroImpl[SumType, VariantType] }

def someMacroImpl[SumType: Type, VariantType: Type](using quotes: Quotes): Expr[Unit] = {
  val discriminatorExpr: Expr[Int] = Expr.summon[DiscriminationCriteria1[SumType]] match {
    case Some(discriminatorCriteriaExpr) =>
      '{ $discriminatorCriteriaExpr.discriminator[VariantType & SumType] }
    case None =>
      // Fall back to the alphanumerical index
      Expr[Int](??? /*alphanumericIndex*/)
  }

  '{ () }
}