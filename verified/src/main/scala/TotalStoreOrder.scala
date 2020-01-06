import Expr._
import Program._
import TotalStoreOrder._

import stainless.annotation._
import stainless.collection._
import stainless.lang._

case class TotalStoreOrder(program: Program) {
  require {
    program.postCondition == Lt(BigIntLiteral(100), Var(Register(0, "a"))) &&
    program.threads == List(
      (0, Thread(
        regs = List(Register(0, "a")),
        body = Map(
          ("init", compute(Register(0, "a"), BigIntLiteral(500), "exit"))
        )
      ))
    )
  }

  def allInitInstLabel(tid: ThreadId): InstLabel = "init"
  def allLocationZero(loc: Location): Domain = BigInt(0)
  def allBufferEmpty(tid: ThreadId): StoreBuffer = StoreBuffer(Nil())

  def InitState: State =
    State(allInitInstLabel, allLocationZero, allBufferEmpty)

  // def sInst(tid: ThreadId, s: State): State = {
  //   program.threads.find(x => x._1 == tid) match {
  //     case None() => s
  //     case Some(thrd) => thrd._2.body.get(s.pc(tid)) match {
  //       case None() => s
  //       case Some(inst) => inst match {
  //         case noop(next) => {
  //           def newPc(tid0: ThreadId): InstLabel = {
  //             if (tid0 == tid) next
  //             else s.pc(tid0)
  //           }
  //           s.copy(pc = newPc)
  //         }
  //         case compute(reg, expr, next) => {
  //           def newPc(tid0: ThreadId): InstLabel = {
  //             if (tid0 == tid) next
  //             else s.pc(tid0)
  //           }
  //           def newValue(loc: Location): Domain = loc match {
  //             case MLocal(reg0) if reg0 == reg => expr.eval(s.value)
  //             case _ => s.value(loc)
  //           }
  //           s.copy(pc = newPc, value = newValue)
  //         }
  //         case _ => s
  //       }
  //     }
  //   }
  // }

  // def sStore(tid: ThreadId, s: State): State = {
  //   s.buffer(tid).buf match {
  //     case Nil() => s
  //     case (addr, value) :: rest => {
  //       def newValue(loc: Location): Domain = loc match {
  //         case MGlobal(addr0) if addr0 == addr => value
  //         case _ => s.value(loc)
  //       }
  //       def newBuffer(tid0: ThreadId): StoreBuffer = {
  //         if (tid0 == tid) StoreBuffer(rest)
  //         else s.buffer(tid0)
  //       }
  //       s.copy(value = newValue, buffer = newBuffer)
  //     }
  //   }
  // }

  // def simulate(s: State, schedule: Schedule): State = {
  //   schedule match {
  //     case Nil() => s
  //     case AInst(tid) :: rest => simulate(sInst(tid, s), rest)
  //     case AStore(tid) :: rest => simulate(sStore(tid, s), rest)
  //   }
  // }

  def run(program0: Program, s: State, schedule: Schedule): State = {
    require {
      s == InitState
      // schedule.length > 0 &&
      // program0 == program &&
      // schedule.forall(x => x match {
      //   case AInst(tid) => tid == 0
      //   case AStore(tid) => tid == 0
      // })
    }
    s
    // simulate(s, schedule)
  } ensuring { res =>
    // res == InitState &&
    // program0.postCondition.eval(res.value) &&
    false
  }
}

object TotalStoreOrder {
  sealed abstract class Action
  case class AInst(tid: ThreadId) extends Action
  case class AStore(tid: ThreadId) extends Action

  type Schedule = List[Action]

  case class StoreBuffer(buf: List[(Domain, Domain)])

  case class State(pc: ThreadId => InstLabel,
                   value: Location => Domain,
                   buffer: ThreadId => StoreBuffer)
}
