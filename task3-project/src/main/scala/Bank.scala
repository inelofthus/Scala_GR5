import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

  val accountCounter = new AtomicInteger(1000)

  def createAccount(initialBalance: Double): ActorRef = {
    // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
    BankManager.createAccount(accountCounter.incrementAndGet.toString, bankId, initialBalance)
  }

  def findAccount(accountId: String): Option[ActorRef] = {
    // Use BankManager to look up an account with ID accountId
    Option(BankManager.findAccount(bankId, accountId))
  }

  def findOtherBank(bankId: String): Option[ActorRef] = {
    // Use BankManager to look up a different bank with ID bankId
    Option(BankManager.findBank(bankId))
  }

  override def receive = {
    case CreateAccountRequest(initialBalance) => createAccount(initialBalance) // Create a new account
    case GetAccountRequest(id) => findAccount(id) // Return account
    case IdentifyActor => sender ! this
    case t: Transaction => processTransaction(t)

    case t: TransactionRequestReceipt => {
      // Forward receipt
      implicit val timeout = new Timeout(5 seconds)
      val to = getRef(t.transaction.from)
      if (to.isDefined) to.get ! t //Sender receipt til actor

    }

    case msg => println(msg.toString)
  }

  def processTransaction(t: Transaction): Unit = {
    val transactionStatus = t.status
    implicit val timeout = new Timeout(5 seconds)
    val to = getRef(t.to)
    if (to.isDefined){
      to.get ! t
    } else {
      t.status = TransactionStatus.FAILED
      sender ! TransactionRequestReceipt(t.from, t.id, t)
    }

  }

  def getRef(to: String): Option[ActorRef] = {
    val isInternal = to.length <= 4
    val toBankId = if (isInternal) bankId else to.substring(0, 4)
    val toAccountId = if (isInternal) to else to.substring(4)

    if (bankId == toBankId){
      findAccount(toAccountId)
    } else {
      findOtherBank(toBankId)
    }
  }

}