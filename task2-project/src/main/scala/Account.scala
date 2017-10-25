import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

  def withdraw(amount: Double): Unit = {
    if(amount>initialBalance){
      throw new NoSufficientFundsException;
    } else if (amount < 0) {
      throw new IllegalAmountException;
    } else {
      initialBalance -= amount
    }
  }
  def deposit(amount: Double): Unit = {
    if (amount >= 0) {
      initialBalance += amount;
    } else {
      throw new IllegalAmountException;
    }
  }
  def getBalanceAmount: Double = initialBalance

  def transferTo(account: Account, amount: Double) = {
    bank addTransactionToQueue (this, account, amount)
  }


}
