import Expr._
import Program._
import TotalStoreOrder._

import stainless.annotation._
import stainless.collection._
import stainless.lang._

case class TotalStoreOrder(program: Program) {
  def allInitInstLabel(tid: ThreadId): InstLabel = "init"
  def allLocationZero(loc: Location): Domain = BigInt(0)
  def allBufferEmpty(tid: ThreadId): StoreBuffer = StoreBuffer(Map())

  def InitState: State =
    State(allInitInstLabel, allLocationZero, allBufferEmpty)

  def simulate(s: State, schedule: Schedule): State = {
    s
  }

  def run(s: State, schedule: Schedule): State = {
    require(s == InitState)
    simulate(s, schedule)
  } ensuring(_ => s == InitState)
}

object TotalStoreOrder {
  sealed abstract class Location 
  case class MGlobal(addr: Domain) extends Location
  case class MLocal(tid: ThreadId, reg: Register) extends Location 

  sealed abstract class Action
  case class AInst(tid: ThreadId) extends Action
  case class AStore(tid: ThreadId) extends Action

  type Schedule = List[Action]

  case class StoreBuffer(buf: Map[Domain, Domain])

  case class State(pc: ThreadId => InstLabel,
                   value: Location => Domain,
                   buffer: ThreadId => StoreBuffer)
}