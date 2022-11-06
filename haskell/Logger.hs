import Data.List.NonEmpty


{-#
newtype Logger a =  Log (NonEmpty a)

instance Functor Logger where
    fmap f (Log ~(a :| as)) = f a :| fmap f as
    --b <$ ~(Log (_ :| as))   = b   :| (b <$ as) --replaces _(and everythning) with b

instance Applicative Logger where
    pure a = Log (a :| [])
    (<*>) = ap
    --liftA2 = liftM2
    
instance Monad Logger where
    (Log ~(a :| as)) >>= f = Log b :| (bs ++ bs')
        where 
            (Log b :| bs) = f a
            --bs = as >>= toList . f
            --toList ~(c :| cs) = c : cs
#-}
--might need [a]->[b]
newtype Logger a b = Log ([b] -> [a])


instance Monad (Logger a) where
    --f :: a -> Logger b c = [b]->[c]
    --Logger a b :: transition :: [a]->[b]
    --result is Logger b c = [b] -> [c]

    --Logger a1 a -> (a -> Logger a1 b) -> Logger a1 b

    --Logger c a -> (a -> Logger c b) -> Logger c b
    --the first parameter is kept!

    --(>>=) (Log transition) f = Log ()
    --return x = Log \x -> (x:[])

    --Logger c a -> (a -> Logger c b) -> Logger c b
    --     [a]->[c]           [b]->[c]       [b]->[c]

    (>>=) (Log transition) f = Log ()
