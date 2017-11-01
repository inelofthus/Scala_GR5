import exceptions._

class Account(val bank: Bank, var initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

  def withdraw(amount: Double): Unit = this.synchronized {
    if(amount>initialBalance){
      throw new NoSufficientFundsException;
    } else if (amount < 0) {
      throw new IllegalAmountException;
    } else {
      initialBalance -= amount
    }
  }
  def deposit(amount: Double): Unit = this.synchronized {
    if (amount >= 0) {
      initialBalance += amount;
    } else {
      throw new IllegalAmountException;
    }
  }
  def getBalanceAmount: Double = initialBalance

  def transferTo(account: Account, amount: Double) = {
    println("transferTo")
    bank addTransactionToQueue (this, account, amount)
  }


}
