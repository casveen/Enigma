{-
The solid state is a construction that models stateful computations where the sequence of states
are already predefined, and can only be put into the next state by calling nextState.

It will for example be used by the enigma, which is a cipher where the encryption changes(the wheels turn, 
and the state goes to the next one) after a single encryption, and it does so in the same manner
no matte rwhat it encrypts. 
Its sequence of states are not dependent on any previous states. It will always be the same sequence(of states!)
no matter the results returned in the states.

How do we want this state to act?
well. the s type should only ever be modified by the nextState command. 
The bind might use the state, but it cannot modify it.

actually, maybe I want an instance Monad Sold<State where a is a monad, 
so that when bind is done we know that only a single state remains regardless of how the bind on a was done.


-}
{-# LANGUAGE InstanceSigs #-}

{-
data SolidState s a = 
    SolidState { 
        runSolidState :: s -> a, 
        nextState :: s -> s
    }

instance Monad (SolidState s) where
    --(>>=) :: SolidState s a -> (a -> SolidState s b) -> SolidState s b
    --(act1 >>= fact2) s = runSolidState act2 is 
    --    where (iv,is) = runSolidState act1 s
    --          act2 = fact2 iv

    --    instance Monad (State s) where  
    return x = 
        let 
            fs :: s -> s
            fs s = s
            --fx :: s -> a
            fx s = x
        in
            SolidState fx fs

    (>>=) :: SolidState s a -> (a -> SolidState s b) -> SolidState s b
    (SolidState f1 n) >>= f = 
        let 
            
            {-(\s -> 
            let 
                (a, newState) = h s  
                (SolidState g n) = f a  
            in  
                g newState)
            n-}
        in 
            SolidState f3 n3 
           

    --(State h) >>= f = State $ \s -> let (a, newState) = h s  
    --                                (State g) = f a  
    --                            in  g newState
-}