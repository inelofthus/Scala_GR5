import scala.annotation.tailrec
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.ExecutionContext
import scala.annotation.tailrec

class Bank(val allowedAttempts: Integer = 3) {

  private val uid = 0
  private var accountUid = 0
  private val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext = ExecutionContext.global

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    println("addTransaction")
    transactionsQueue push new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
  }

  def generateAccountId: Int = this.synchronized {
    val freshUid = accountUid + 1
    accountUid = freshUid
    freshUid

  }

  Main.thread(processTransactions)
  @tailrec
  private def processTransactions: Unit = {
    println("processTransactions")
    executorContext.execute(transactionsQueue.pop)
    Thread.sleep(10)
    processTransactions
  }

  def addAccount(initialBalance: Double): Account = {
    println("addAccount")
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    //println(transactionsQueue.iterator.toList.size)
    processedTransactions.iterator.toList
  }

}
