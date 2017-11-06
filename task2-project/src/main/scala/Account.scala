import exceptions._

class Account(val bank: Bank, val initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

  def withdraw(amount: Double): Unit = this.synchronized {
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

  def transferTo(account: Account, amount: Double) = {

    bank addTransactionToQueue (this, account, amount)
  }


}
