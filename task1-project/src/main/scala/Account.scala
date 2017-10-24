class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {
  def withdraw(amount: Double): Unit = {
    initialBalance-=amount;
  }
  def deposit(amount: Double): Unit = {
    initialBalance+=amount;
  }
  def getBalanceAmount: Double = {
    return initialBalance;
  }
}
