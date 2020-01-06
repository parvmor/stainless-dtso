import Expr._
import Program._
import TotalStoreOrder._

import stainless.annotation._
import stainless.collection._
import stainless.lang._

case class TotalStoreOrder(program: Program) {
  require {
    program.postCondition == Lt(BigIntLiteral(1), Var(Register(BigInt(0), "a"))) &&
    program.threads == List(
      (BigInt(0), Thread(
        regs = List(Register(BigInt(0), "a")),
        body = Map(
          ("init", compute(Register(BigInt(0), "a"), BigIntLiteral(5), "exit"))
        )
      ))
    )
  }

  def allInitInstLabel(tid: ThreadId): InstLabel = "init"
  def allLocationZero(loc: Location): Domain = BigInt(0)
  def allBufferEmpty(tid: ThreadId): StoreBuffer = StoreBuffer(Nil())

  def InitState: State =
    State(allInitInstLabel, allLocationZero, allBufferEmpty)

  def initNotInExit(s: State): Unit = {
    require(s == InitState)
  } ensuring(_ => program.threads.forall(x => s.pc(x._1) == "init" && s.pc(x._1) != "exit"))

  def sInst(tid: ThreadId, s: State): State = {
    program.threads.find(x => x._1 == tid) match {
      case None() => s
      case Some(thrd) => thrd._2.body.get(s.pc(tid)) match {
        case None() => s
        case Some(inst) => inst match {
          case noop(next) => {
            def newPc(tid0: ThreadId): InstLabel = {
              if (tid0 == tid) next
              else s.pc(tid0)
            }
            s.copy(pc = newPc)
          }
          case compute(reg, expr, next) => {
            def newPc(tid0: ThreadId): InstLabel = {
              if (tid0 == tid) next
              else s.pc(tid0)
            }
            def newValue(loc: Location): Domain = loc match {
              case MLocal(reg0) if reg0 == reg => expr.eval(s.value)
              case _ => s.value(loc)
            }
            s.copy(pc = newPc, value = newValue)
          }
          case _ => s
        }
      }
    }
  }

  def sStore(tid: ThreadId, s: State): State = {
    s.buffer(tid).buf match {
      case Nil() => s
      case (addr, value) :: rest => {
        def newValue(loc: Location): Domain = loc match {
          case MGlobal(addr0) if addr0 == addr => value
          case _ => s.value(loc)
        }
        def newBuffer(tid0: ThreadId): StoreBuffer = {
          if (tid0 == tid) StoreBuffer(rest)
          else s.buffer(tid0)
        }
        s.copy(value = newValue, buffer = newBuffer)
      }
    }
  } ensuring { res =>
    program.threads.forall(x => res.pc(x._1) == s.pc(x._1))
  }

  def pcPreservedByStore(s: State, ns: State, tid: ThreadId): Unit = {
    require(ns == sStore(tid, s) && program.threads.exists(x => s.pc(x._1) != "exit"))
  } ensuring { _ =>
    program.threads.forall(x => s.pc(x._1) == ns.pc(x._1)) &&
    program.threads.exists(x => ns.pc(x._1) != "exit")
  }

  def simulate(s: State, schedule: Schedule): State = {
    require {
      program.threads.exists(x => s.pc(x._1) != "exit")
    }
    schedule match {
      case Nil() => s
      case AInst(tid) :: rest => {
        val ns: State = sInst(tid, s)
        if (program.threads.forall(x => ns.pc(x._1) == "exit")) ns
        else simulate(ns, rest)
      }
      case AStore(tid) :: rest => {
        val ns: State = sStore(tid, s)
        pcPreservedByStore(s, ns, tid)
        simulate(ns, rest)
      }
    }
  } ensuring { res =>
    program.threads.exists(x => res.pc(x._1) != "exit") ||
    program.postCondition.eval(res.value)
  }

  def run(s: State, schedule: Schedule): State = {
    require {
      s == InitState &&
      schedule.forall(x => x match {
        case AInst(tid) => program.threads.exists(x => x._1 == tid) 
        case AStore(tid) => program.threads.exists(x => x._1 == tid) 
      })
    }
    initNotInExit(s)
    simulate(s, schedule)
  } ensuring { res =>
    program.threads.exists(x => res.pc(x._1) != "exit") ||
    program.postCondition.eval(res.value)
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
