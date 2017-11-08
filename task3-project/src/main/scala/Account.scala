import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

  private var transactions = HashMap[String, Transaction]()

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  def getFullAddress: String = {
    bankId + accountId
  }

  def getTransactions: List[Transaction] = {
    transactions.valuesIterator.toList
  }

  def allTransactionsCompleted: Boolean = {
    !getTransactions.exists(p => p.status == TransactionStatus.PENDING)
  }

  def withdraw(amount: Double): Unit =  this.synchronized {
    if(amount>balance.amount){
      throw new NoSufficientFundsException;
    } else if (amount < 0) {
      throw new IllegalAmountException;
    } else {
      balance.amount -= amount
    }
  }

  def deposit(amount: Double): Unit = this.synchronized {
    if (amount >= 0) {
      balance.amount += amount;
    } else {
      throw new IllegalAmountException;
    }
  }

  def getBalanceAmount: Double = balance.amount

  def sendTransactionToBank(t: Transaction): Unit = {
    // Should send a message containing t to the bank of this account
    try {
      BankManager.findBank(bankId) ! t
    } catch {
      case _: NoSuchElementException => t.status = TransactionStatus.FAILED
    }
  }

  def transferTo(accountNumber: String, amount: Double): Transaction = {

    val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

    if (reserveTransaction(t)) {
      try {
        withdraw(amount)
        sendTransactionToBank(t)

      } catch {
        case _: NoSufficientFundsException | _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
      }
    }

    t

  }

  def reserveTransaction(t: Transaction): Boolean = {
    if (!transactions.contains(t.id)) {
      transactions += (t.id -> t)
      return true
    }
    false
  }

  override def receive = {
    case IdentifyActor => sender ! this

    case TransactionRequestReceipt(to, transactionId, transaction) => {
      // Process receipt
      if (transaction.status == TransactionStatus.FAILED){
        deposit(transaction.amount)
      }
      transaction.receiptReceived = true
    }

    case BalanceRequest => sender ! getBalanceAmount

    case t: Transaction => {
      // Handle incoming transaction
      deposit(t.amount)
      t.status = TransactionStatus.SUCCESS
      sender ! TransactionRequestReceipt(t.from, t.id, t)
    }

    case msg => println(msg.toString)
  }


}
