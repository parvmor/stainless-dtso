import Expr._

case class Register(name: String)

sealed abstract class ArithExpr {
  def eval(value: Register => Domain): Domain = this match {
    case BigIntLiteral(i) => i
    case Var(id) => value(id)
    case UnaryMinus(e) => -e.eval(value)
    case Plus(e1, e2) => e1.eval(value) + e2.eval(value)
    case Minus(e1, e2) => e1.eval(value) - e2.eval(value)
    case Mult(e1, e2) => e1.eval(value) * e2.eval(value)
    case Mod(e1, e2) => {
      val e2Value: Domain = e2.eval(value)
      if (e2Value == 0) 0
      else e1.eval(value) % e2Value
    }
    case Div(e1, e2) => {
      val e2Value: Domain = e2.eval(value)
      if (e2Value == 0) 0
      else e1.eval(value) / e2Value
    }
  }
}

case class BigIntLiteral(i: BigInt) extends ArithExpr
case class Var(id: Register) extends ArithExpr
case class UnaryMinus(e: ArithExpr) extends ArithExpr
case class Plus(e1: ArithExpr, e2: ArithExpr) extends ArithExpr
case class Minus(e1: ArithExpr, e2: ArithExpr) extends ArithExpr
case class Mult(e1: ArithExpr, e2: ArithExpr) extends ArithExpr
case class Mod(e1: ArithExpr, e2: ArithExpr) extends ArithExpr
case class Div(e1: ArithExpr, e2: ArithExpr) extends ArithExpr

sealed abstract class BoolExpr {
  def eval(value: Register => Domain): Boolean = this match {
    case BooleanLiteral(b) => b
    case Lt(e1, e2) => e1.eval(value) < e2.eval(value)
    case Leq(e1, e2) => e1.eval(value) <= e2.eval(value)
    case Gt(e1, e2) => e1.eval(value) > e2.eval(value)
    case Geq(e1, e2) => e1.eval(value) >= e2.eval(value)
    case Equals(e1, e2) => e1.eval(value) == e2.eval(value)
    case Not(e) => !e.eval(value)
    case And(e1, e2) => e1.eval(value) && e2.eval(value)
    case Or(e1, e2) => e1.eval(value) || e2.eval(value)
  }
}

case class BooleanLiteral(b: Boolean) extends BoolExpr
case class Lt(e1: ArithExpr, e2: ArithExpr) extends BoolExpr
case class Leq(e1: ArithExpr, e2: ArithExpr) extends BoolExpr
case class Gt(e1: ArithExpr, e2: ArithExpr) extends BoolExpr
case class Geq(e1: ArithExpr, e2: ArithExpr) extends BoolExpr
case class Equals(e1: ArithExpr, e2: ArithExpr) extends BoolExpr
case class Not(e: BoolExpr) extends BoolExpr
case class And(e1: BoolExpr, e2: BoolExpr) extends BoolExpr
case class Or(e1: BoolExpr, e2: BoolExpr) extends BoolExpr

object Expr {
  type Domain = BigInt
}