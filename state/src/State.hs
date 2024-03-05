module State (State, extract, inject, modify, runState, evalState, execState) where

import Control.Monad (ap, liftM)

{-
 -  State is able to "store" in the same way a DList is - composition of functions.
 -
 -  We have `s -> (a, s)`. 
 -
 -  `a` is our "result", like if we had IO Int, `a` would be int here.
 -
 -  Similarly, `s` is the actual *state* we're trying to store.
 -
 -
 -  Again like DList, we actually run our state by giving it the 
 -  initial "seed", as an example for DList we give it the initial list
 -
 -  e.g.
 -  (\x -> 3 : 4 : x) [] == [3, 4]
 -
 -  And for our state, we could give it the initial state value
 -
 -           state computation
 -                   v
 -  runState (S $ \s -> (3, s)) "hello" == (3, "hello")
 -                                 ^
 -                          initial value of s
 -
 -  Now, the question might arise - *why* are we representing state in this way? 
 -  With DLists, our motivation was efficiency - we get appending to be O(1) 
 -  amortized instead of O(n) - but for us, State isn't a matter of efficiency. 
 -  For State, by representing it this way, something of type `State a b` is 
 -  actually a *computation* using State. That is to say, `runState` actually runs 
 -  some 'computation' that we're using State for - instead of just storing state, 
 -  we store what we're *doing* with a given state, which is also why we need the 
 -  initial seed value - to run our computation on. Using this, we're actually able 
 -  to chain our actions! This lets us write stateful code, chain it like 
 -  normal imperative code, *and* keep our code 'pure' - there's no global side effects!
 -
 -  As an example, consider something which
 -  stores what numbers we've already seen in a list and
 -  then only processes numbers it hasn't seen.
 -
 -  So our actual store could be say a map from Int to Bool,
 -  where the keys are what we've seen and the values are the results
 -
 -  For the results here, let's say we're computing whether or not it's prime
 -
 -  --
 -  -- Here, we have isPrime, which "returns" a bool
 -  -- and has an internal state of the described map.
 -  --
 -  -- Now, note that it returns something of type State,
 -  -- which can be a bit confusing - where does it take in the state?
 -  -- But what you need to remember is that State itself stores a function,
 -  -- so by returning a State, we essentially return what is actually
 -  -- a *computation* with state, like we said earlier. So in summary,
 -  -- `isPrime` takes in an integer, and then gives you back a 
 -  -- `State Bool (Map Int Bool)`, which in essense, is a function which
 -  -- takes in some value for the state, and then uses it, maybe updating
 -  -- the state along the way.
 -  isPrime :: Int -> State Bool (Map Int Bool)
 -  -- Now `s` here is the *previous state*, which makes sense,
 -  -- as this is a computation - the function tells us how to 
 -  -- run 'isPrime' given some value of state, and what to do
 -  -- to that value.
 -  isPrime n = S $ \s -> (
 -      if M.contains s n then
 -          (True, s)
 -      else (
 -          let computed = isPrime' n in 
 -          (computed, M.insert s (n, computed))
 -      )
 -  )
 -
 -
 -  Now, expanding a bit on this example, we have our constructor S
 -  which we give the function \s -> (...) - this function takes
 -  in some state `s`, and then as the state here is the Map,
 -  we're able to use it to check if we already know if the number is
 -  prime. Otherwise, we check if it's prime ourselves, and then
 -  insert it into the map. After doing this, we're able to
 -  give back the updated map.
-}
newtype State s a = S { runState :: s -> (a, s) }

{- 
 - Now, we implement Functor, Applicative, and Monad for
 - State, so we can use the properties we've discussed in
 - a convenient manner
 -}
instance Functor (State s) where
  -- So in this, we run the given computation `st` with the state `s`,
  -- and then we keep the new value of the state, while applying f
  -- to the result
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (S st) = S $ \s -> let (res, state) = st s in (f res, state)

{-
 -  Our pure instance, S $ \y -> (x, y)
 -  takes the value we're given in pure - `x`
 -  and makes that our result.
 -}
instance Applicative (State s) where
  pure :: a -> State s a
  pure x = S $ \y -> (x, y)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (S f) <*> (S v) = S $ \s -> 
    -- First, we run the left to get our `(a -> b)`
    let (f', st) = f s in
    -- then, with the updated state, we run the right
    -- to get our value `a`
    let (res, st') = v st in
    -- Finally, we can apply the function we got
    -- to the `a` we got to get `b`, and chain the
    -- state through again.
    (f' res, st')

{-
 -
 -}

{-
 -  Now the fun part, bind! We need to take some state computation
 -  and a function giving us a state computation - and then chain them together.
 -
 -  Remember that the `a` in `State s a` is the actual "result", and so here
 -  we take the result from our first computation (and the associated state)
 -  and feed it into the next function, also giving it the seed state it needs
 -  by taking it from the first function!
 -}
instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  -- This could be an exercise idk
  st >>= f = S $ \s -> 
    let (a, s') = runState st s
    in runState (f a) s'


{-
 - We can write two things to actually fully run our computation, the first
 - being evalState, which only gives back the result
 -}
evalState :: State s a -> s -> a
evalState st initial = fst $ runState st initial
{-
 - And the second exec, which only gives back the state, discarding the result.
 -}
execState :: State s a -> s -> s
execState st initial = snd $ runState st initial


{-
 - These are probably good as exercises, given the above but I've left 
 - the solutions in for now.
 -
 - `extract` and `inject` could be given or done as an exercise I think,
 -
 - but `modify`, `swap`, and `returnAndModify` follow from them fairly 
 - intuitively (I think), and can also be written in terms of them.
 -}


{-
 - `extract` serves to get us the value of the state back. But then, 
 -  why is it still a `State`? The purpose here is to be able to use
 -  this in the context of `State` the `Monad`. Thinking of like a 
 -  do block, you could have
 -
 -
 -  f x = do
 -      v <- extract
 -
 -  And now `v` would have the value of the state.
 -
 -  This is indicated within the type signature, the "result" of our 
 -  stateful computation is in fact the state itself!
 -
 -  As another example, think back to our first example `isPrime`.
 -  We can write this much more simply using the functions below 
 -
 -  isPrime :: Int -> State (Map Int Bool) Bool
 -  isPrime n = do
 -      -- Get our map
 -      m <- extract
 -
 -      -- Now, we check if we already know if it's prime
 -      case M.lookup m n of
 -          -- Since the map contains a result, we can just
 -          -- use that!
 -          Just v -> pure v
 -
 -          -- Otherwise, we have to actually check ourselves
 -          Nothing -> do
 -              let res = isPrime' n
 -
 -              -- After we check, we put it into the map
 -              -- for future use, and then update our state
 -              inject $ M.insert m (n, res)
 -
 -              pure res
 -}
extract :: State s s
extract = undefined -- S $ \s -> (s, s)


{-
 - `inject` lets us "set" the state
 -
 - That's why we take in `s`, our new state,
 - and give back a computation with the desired state,
 - and the result `()`. The result is `()` as `inject` doesn't 
 - actually do anything besides set the state, so we don't have 
 - anything to return. The upside of this is we don't have to 
 - take in some State, and thus when we use this in say, a do block 
 - or a bind, we only have to give it the new state as parameter 
 - directly, and not the previous computation
 -}
inject :: s -> State s ()
inject s' = undefined -- S $ const ((), s')

{-
 - `modify` is really similar to `inject`, except now instead 
 - of *replacing* the state, as the name implies, we modify it.
 - Thus we take in some modifier function (s -> s), and apply 
 - that to our state.
 -}
modify :: (s -> s) -> State s ()
modify f = undefined -- S $ ((),) . f

{-
 - `swap` allows us to swap out our state with something we give, 
 - but it *also* gives us back the previous state.
 -
 - It's very similar to inject, but just has this extra value back.
 -}
swap :: s -> State s s
swap s' = undefined -- S $ \s -> (s, s')

{-
 - This one is a bit like a combination of `swap` and `modify`. 
 - Instead of swapping out the old state
 - with an entirely new one, we instead apply a modification `f` 
 - (like `modify`), but similar to `swap`, we give back our old state.
 -}
returnAndModify :: (s -> s) -> State s s
returnAndModify f = undefined -- S $ (\s -> (s, f s))


{-
 - This is just a bit more convenience over `extract`. We apply 
 - some "projection" onto the state itself, and then get back the 
 - result of that. In other words, we can just apply something to the
 - internal state, and get back the result directly - so instead of say
 -
 -  do 
 -      v <- extract
 -      v' <- M.get v 37
 -
 -  we could do
 -
 -  do
 -      v <- extracts ((flip M.get) 37)
 -
 -  and just get back whatever value our map has for 37, 
 -  supposing here that the store is a map.
 -}
extracts :: (s -> a) -> State s a
extracts f = undefined


{-
 - Thinking we could have something where we do something
 - with an explicit state as a parameter, and then write
 - it again using State itself, so that they can get a
 - better understanding of how you'd use it and how
 - it can be simpler than having to lug around the parameter
 -}
