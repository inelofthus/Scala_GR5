import scala.annotation.tailrec
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import exceptions._

class Bank(val allowedAttempts: Integer = 3) {

  private val uid = 0
  private var accountUid = 0
  private val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext = ExecutionContext.fromExecutorService(new ForkJoinPool())

  executorContext.execute(new Runnable {
    override def run(): Unit = processTransactions
  })

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    println("addTransaction")
    transactionsQueue push new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
    println(transactionsQueue.iterator.toList.size)
  }

  def generateAccountId: Int = this.synchronized {
    val freshUid = accountUid + 1
    accountUid = freshUid
    freshUid

  }

  private def processTransactions: Unit = {

    while(true) {

      val transaction = transactionsQueue.pop

      try {
        transaction.run()
        transaction.status = TransactionStatus.SUCCESS
      }
      catch {
        case e: IllegalAmountException => {
          transaction.status = TransactionStatus.FAILED
        }
        case e: NoSufficientFundsException => {
          transaction.allowedAttempts -= 1
          if (transaction.allowedAttempts == 0) {
            transaction.status = TransactionStatus.FAILED
          }
          else {
            transactionsQueue.push(transaction)
          }
        }
      } finally {
        if (transaction.status != TransactionStatus.PENDING){
          processedTransactions.push(transaction)

        }
      }
    }

  }

  def addAccount(initialBalance: Double): Account = {

    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    //println(transactionsQueue.iterator.toList.size)
    processedTransactions.iterator.toList
  }

}
