module Data.LinkedList where

import Prelude (class Functor, class Show, show, ($), (+), (<>))

data LinkedList a
  = Empty
  | Head a (LinkedList a)

instance showLinkedList :: Show a => Show (LinkedList a) where
  show = f "["
    where
    f x Empty = x <> "]"
    f x (Head y Empty) = f (x <> show y) Empty
    f x (Head y z) = f (x <> show y <> ",") z

make :: ∀ a. a -> LinkedList a
make x = Head x Empty

prepend :: ∀ a. a -> LinkedList a -> LinkedList a
prepend x y = Head x y

length :: ∀ a. LinkedList a -> Int
length = f 0
  where
  f x Empty = x
  f x (Head _ z) = f (x + 1) z

reverse :: ∀ a. LinkedList a -> LinkedList a
reverse xs = f xs Empty
  where
  f Empty z = z
  f (Head x y) z = f y (prepend x z)

fold :: ∀ a b. (a -> b -> a) -> a -> LinkedList b -> a
fold fn x Empty = x
fold fn x (Head y z) = fold fn (fn x y) z

instance functorLinkedList :: Functor LinkedList where
  map fn xs = reverse $ fold (\a b -> prepend (fn b) a) Empty xs