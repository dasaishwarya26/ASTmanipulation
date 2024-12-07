module Bank where

-- Define a stateful BankOp using the State monad
import Control.Monad.State

-- BankOp encapsulates operations on a bank account balance
newtype BankOp a = BankOp { runBank :: State Float a }
    deriving (Functor, Applicative, Monad)

-- Deposit function: adds the given amount to the balance
deposit :: Float -> BankOp ()
deposit amount = BankOp $ modify (+ amount)

-- Withdraw function: subtracts the given amount, allowing overdrafts up to -100
withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ do
    balance <- get
    let actualWithdrawal = if balance - amount < -100
                           then balance + 100 -- Allow overdraft up to -100
                           else amount
    put (balance - actualWithdrawal)
    return actualWithdrawal

-- Get the current balance
getBalance :: BankOp Float
getBalance = BankOp get

-- Run a BankOp operation
runBankOp :: BankOp a -> a
runBankOp (BankOp op) = evalState op 0 -- Initialize with a starting balance of 0
