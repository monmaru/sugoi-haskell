import Control.Applicative

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs