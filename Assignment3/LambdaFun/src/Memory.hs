{-
 * This file has been copied and adapted from the materials written by 
 * Mike Spivey (https://spivey.oriel.ox.ac.uk/corner/Welcome_to_Spivey%27s_Corner)
 * for the POPL course at the Oxford University CS department.
-}

module Memory(Location(..), Memory(..), init_mem, contents, update, fresh,
        array, index, dump) where

import qualified Data.Map as Map

newtype Location = Loc Integer deriving Eq

instance Show Location where show (Loc u) = show u

-- Invalid locations can be non-existent, or they can exist 
-- but be uninitialised.  You cant update a non-existent location,
-- and you cant get the contents of an uninitialised location.

newtype Memory a = Mem (Integer, Map.Map Integer (Maybe a))

init_mem :: Memory a
init_mem = Mem (0, Map.empty)

contents :: Memory a -> Location -> a
contents (Mem (n, s)) (Loc u) = 
  case Map.lookup u s of
    Just (Just v) -> v
    Just Nothing -> error ("uninitialized location " ++ show u)
    Nothing -> error ("non-existent location " ++ show u)

update :: Memory a -> Location -> a -> Memory a
update (Mem (n, s)) (Loc u) v = 
  case Map.lookup u s of
    Just _ -> Mem (n, Map.insert u (Just v) s)
    Nothing -> error ("non-existent location " ++ show u)

fresh :: Memory a -> (Location, Memory a)
fresh (Mem (n, s)) = (Loc n, Mem (n+1, Map.insert n Nothing s))

array :: Memory a -> Integer -> (Location, Memory a)
array (Mem (n, s)) k = (Loc n, Mem (n+k, s))

index :: Memory a -> Location -> Integer -> Location
index mem (Loc a) i = Loc (a+i)

dump :: Show a => Memory a -> String
dump (Mem (n, s)) = show (Map.toList s)