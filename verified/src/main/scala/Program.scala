import Expr._
import Program._

import stainless.annotation._
import stainless.collection._
import stainless.lang._

case class Thread(
  regs: List[Register],
  body: Map[InstLabel, Statement]
)

case class Program(
   threads: List[(ThreadId, Thread)],
   postCondition: BoolExpr
)

object Program {
  type InstLabel = String

  sealed abstract class Statement
  case class noop(next: InstLabel) extends Statement
  case class mfence(next: InstLabel) extends Statement
  case class assert(expr: BoolExpr, next: InstLabel) extends Statement
  case class compute(reg: Register, expr: ArithExpr, next: InstLabel) extends Statement
  case class load(reg: Register, addr: ArithExpr, next: InstLabel) extends Statement
  case class store(addr: ArithExpr, value: ArithExpr, next: InstLabel) extends Statement
}