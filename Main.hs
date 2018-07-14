main :: IO ()
main =  do
    print fibsLessThanThreshold
    print (largestPrimeFactor 100)
    print (findMin 20)

fibs :: [Int]
fibs = 1 : 2 : zipWith ( + ) fibs (tail fibs)

fibsLessThanThreshold :: [Int]
fibsLessThanThreshold =
  filter (\x -> mod x 2 == 1)
    (takeWhile (< 4000000) fibs)

primes :: [Int]
primes = filter (\x -> largestPrimeFactor x == x) [2..]

largestPrimeFactor  :: Int -> Int
largestPrimeFactor 2 = 2
largestPrimeFactor 3 = 3
largestPrimeFactor n =
 let
   efficientPrimes = takeWhile (\x -> x * x <= n) primes
   isMod x =  mod n x == 0
 in
   case
     find
       isMod
       efficientPrimes of
     Nothing -> n
     Just x ->
       let rest = div n x
        in
        if rest <= x
           then x
           else
           max x (largestPrimeFactor rest)

find :: (t -> Bool) -> [t] -> Maybe t
find _ [] = Nothing
find f (a : xs) =
  if f a
     then Just a
     else find f xs

findMin :: Int -> Int
findMin 1 = 1
findMin 2 = 2
findMin n =
  lcm n (findMin (n-1))


class Monad m => MonadPlus m where  
    mzero :: m a  
    mplus :: m a -> m a -> m a  
instance MonadPlus [] where  
    mzero = []  
    mplus = (++)  
guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero  

sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x) >> return x  

