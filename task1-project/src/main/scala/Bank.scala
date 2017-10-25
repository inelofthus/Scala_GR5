import exceptions.NoSufficientFundsException, exceptions.IllegalAmountException

object Bank {

  private var idCounter: Int = 0

  def transaction(from: Account, to: Account, amount: Double): Unit = {
    if (amount >= 0) {
      try {
        from.withdraw(amount);
        to.deposit(amount);
      } catch{
        case nsfe: NoSufficientFundsException => throw new NoSufficientFundsException;
      }
    } else {
      throw new IllegalAmountException;
    }
  } // Implement

  def getUniqueId: Int = {
    idCounter += 1 // Can this be improved?
    idCounter
  }
}

