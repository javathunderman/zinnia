{-# LANGUAGE FlexibleInstances #-}

module ErrState where

import Control.Monad (ap, liftM)
import qualified Data.Set as Set

{-
 - Here, we import the State we defined from State.hs, though we do
 - so with a starting S
 -}
import qualified State as S 

data Expr = Div Expr Expr | Literal Int

data DivErr = DivByZero | IllegalDiv Int

{- 
 - Suppose we had some division function `eval`, which runs 
 - an expression - this expression can be one of two things:
 - a literal, which we just give back the value for, or division
 - of two numbers. Divison is allowed *unless* we're diving by 
 - zero *or* we try to divide by an integer we've disallowed. 
 - Additionally, whenever we do a division, we disallow dividing 
 - by the denominator we used for future divisions. Here, we have 
 - State hold a `Set Int`, which contains the disallowed denominators.
 -}
eval :: Expr -> S.State (Set.Set Int) (Either DivErr Int)
eval (Literal i) = pure $ Right i
eval (Div a b) = do
    s <- S.extract

    a' <- eval a

    case a' of
        Left err -> pure $ Left err
        Right aVal -> do
            b' <- eval b
            case b' of
                Left err -> pure $ Left err
                Right bVal ->
                    
                    if bVal == 0 then
                        pure $ Left DivByZero
                    else
                        if Set.member bVal s then 
                            pure $ Left (IllegalDiv bVal) 
                        else do
                            S.inject (Set.insert bVal s)
                            pure $ Right (aVal `div` bVal)



{-
 -  State is great, but as we say above, we have to handle errors
 -  ourselves again. Instead, we can write a type which
 -  has both State *and* the ability to handle errors.
 -
 -  Similarly to State, we have `a` - our result,
 -  `s` - our state, and now we have `e` - the 
 -  error type.
 -}
newtype ErrS e s a = E { runErrS :: s -> Either e (a, s) }

{-
 - In doing this, we're generalizing the idea of errors and state,
 - so let's make a type-class for this.
 -}

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

class Monad m => MonadError e m where
    throw :: e -> m a

{-
 - As an example, we can implement some of this for
 - our own State, and for the default Monad we use for
 - error-handling - Either
 -}
instance MonadState s (S.State s) where
    get = S.extract
    put = S.inject

instance MonadError e (Either e) where
    throw = Left

{-
 - A fun thing with Monad is that there are the
 - functions `ap` and `liftM`, which are equivalent to
 - `<*>` and `fmap`! These are implemented internally
 - using only bind, so when we write a Monad instance,
 - we can just use these and save ourselves some hassle.
 -}
instance Functor (ErrS e s) where
    fmap = liftM

instance Applicative (ErrS e s) where
    pure v = E $ \s -> Right (v, s)
    (<*>) = ap

-- Do we want them to write this? It's kinda annoying so not entirely sure.
instance Monad (ErrS e s) where
    l >>= r = E $ \s -> case runErrS l s of
        Left err -> Left err
        Right (a, s') -> runErrS (r a) s'

-- Exercise
instance MonadState s (ErrS e s) where
    get = undefined
    put = undefined

instance MonadError e (ErrS e s) where
    throw = undefined
    

{-
 - Now, we can rewrite eval using our new ErrS monad! This
 - one is *much* simpler than our previous code.
 -}

eval' :: Expr -> ErrS DivErr (Set.Set Int) Int
eval' (Literal i) = pure i
eval' (Div a b) = do
    m :: (Set.Set Int) <- get

    a' <- eval' a
    b' <- eval' b

    -- Now, you finish the method.
    undefined
