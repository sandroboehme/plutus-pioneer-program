module Week04.Writer where

import Control.Monad
import Week04.Monad

data Writer a = Writer a [String]
    deriving Show

number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]

tell :: [String] -> Writer ()
tell = Writer ()

foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  let
    s = k + l + m
    Writer _ us = tell ["sum: " ++ show s]
  in
    Writer s $ xs ++ ys ++ zs ++ us

sandrosFoo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
sandrosFoo (Writer k xs) (Writer l ys) (Writer m zs) =
  Writer (k + l + m) $ xs ++ ys ++ zs ++ ["sum: " ++ show (k + l + m)]


sandrosFoo' :: [Writer Int] -> Writer Int
sandrosFoo' (firstWriter:remainingWriters) = foldl (\accWriter currentWriter ->
      let
        Writer sum accLogs = accWriter
        Writer num currLogs = currentWriter
      in
        Writer (sum + num) (accLogs ++ currLogs)
  ) firstWriter remainingWriters

concatLogs :: Writer a -> Writer a -> Writer a
concatLogs leftWriter rightWriter =
      let
        Writer leftNum leftLogs = leftWriter
        Writer rightNum rightLogs = rightWriter
      in
        Writer rightNum (leftLogs ++ rightLogs)

bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a -- executes the function with `a` as parameter (?) and deconstructs the result to the left
  in
    Writer b $ xs ++ ys

foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k -> y `bindWriter` \l ->
             z `bindWriter` \m ->
                let s = k + l + m
                in tell ["sum: " ++ show s] `bindWriter` \_ -> Writer s []

fooSandro :: Writer Int -> Writer Int -> Writer Int -> Writer Int
fooSandro w1 w2 w3 =
             w1 `concatLogs`
             w2 `concatLogs`
             w3 `concatLogs`
                let
                  Writer w1Num _ = w1
                  Writer w2Num _ = w2
                  Writer w3Num _ = w3
                  s = (w1Num + w2Num + w3Num)
                in Writer s ["sum: " ++ show s]

foo''Bind :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo''Bind x y z = threeInts x y z >>= \s ->
                  tell ["sum: " ++ show s] >>
                  return s

foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = do
    s <- threeInts x y z
    tell ["sum: " ++ show s]
    return s

instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure = return
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
