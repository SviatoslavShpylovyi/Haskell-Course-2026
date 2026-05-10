newtype Reader r a = Reader {runReader::r->a}

instance Functor (Reader r ) where
    fmap f (Reader g) =
        Reader(\r -> f(g r))
instance Applicative (Reader r) where
    pure x = Reader(\_ ->x)

    liftA2 f (Reader ra) (Reader rb) =
        Reader(\r->f(ra r) (rb r))
instance Monad (Reader r) where
    (Reader ra) >>= f = 
        Reader(\r ->
            let a = ra r
                Reader rb = f a 
            in rb r)

ask:: Reader r r
ask= Reader id

asks::(r->a)->Reader r a
asks f = Reader f

local ::(r -> r) -> Reader r a -> Reader r a 
local f (Reader ra) = Reader (\r -> ra(f r))

data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)
data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)

calculateInterest:: Account ->Reader BankConfig Int
calculateInterest acc = do
    rate<-asks interestRate
    pure(round(fromIntegral(balance acc)*rate))
applyTransactionFee::Account -> Reader BankConfig Account
applyTransactionFee acc = do
    fee<-asks transactionFee
    pure acc { balance = balance acc -fee}
checkMinimumBalance :: Account ->Reader BankConfig Bool
checkMinimumBalance acc = do
    min<-asks minimumBalance
    pure(balance acc>= min)
processAccount :: Account ->Reader BankConfig (Account, Int,Bool)
processAccount acc = do
    update<-applyTransactionFee acc
    interest <- calculateInterest acc
    valid <-checkMinimumBalance acc
    pure (update,interest,valid )
    