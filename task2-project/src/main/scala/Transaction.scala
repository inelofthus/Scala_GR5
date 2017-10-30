import exceptions._

import scala.collection.mutable
import java.util.concurrent
import java.util.concurrent.LinkedBlockingQueue
import scala.collection
import scala.collection.JavaConversions._

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  private val queue = new LinkedBlockingQueue[Transaction]()

  // Remove and return the first element from the queue
  def pop: Transaction = queue.take

  // Return whether the queue is empty
  def isEmpty: Boolean = queue.peek == null

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = queue.offer(t)

  // Return the first element from the queue without removing it
  def peek: Transaction = queue.peek

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = queue.iterator
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {

    def doTransaction() = {
      from withdraw amount
      to deposit amount
    }

    if (from.uid < to.uid) from synchronized {
      to synchronized {
        doTransaction
      }
    } else to synchronized {
      from synchronized {
        doTransaction
      }
    }

    doTransaction()
    // Extend this method to satisfy new requirements.

  }
}
