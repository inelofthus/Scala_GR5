import exceptions.NoSufficientFundsException, exceptions.IllegalAmountException

class Account(var initialBalance: Double, val uid: Int = Bank getUniqueId) {
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
  } // Implement
  def getBalanceAmount: Double = initialBalance // Implement
}
