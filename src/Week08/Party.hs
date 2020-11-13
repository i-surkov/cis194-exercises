module Week08.Party
  ( glCons,
    moreFun,
    nextLevel,
    maxFun,
    main,
  )
where

import Data.Tree (Tree, foldTree)
import Week08.Employee
  ( Employee (..),
    GuestList (..),
  )

--------------------------- Exercise 1

-- | Add employee to guest list
glCons :: Employee -> GuestList -> GuestList
glCons emp gl = GL [emp] (empFun emp) <> gl

-- See src/Week08/Employee.hs to implement
-- the monoid instance for GuestList.
-- Avoids orphans.

-- | Compare by fun, employing the fact that `GuestList` is a part of `Ord`
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--------------------------- Exercise 2

-- foldTree is defined in Data.Tree, go read it if you like

--------------------------- Exercise 3

-- | Going one level up in hierarchy - one the left, guest list that
-- includes the boss of that subtree, on the right, the one that doesn't
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results =
  (glCons boss . mconcat . map snd $ results, mconcat . map fst $ results)

--------------------------- Exercise 4

-- | Generates the most optimal guest list from the given tree,
-- maximised by fun being had
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . foldTree nextLevel

--------------------------- Exercise 5

-- | Parse a tree from file, calculate optimal guest list and
-- print fun value as well as first 10 guests from the list
main :: IO ()
main = do
  gl <- maxFun . read <$> readFile "./resources/Week08/company.txt"
  putStrLn ("Total fun: " ++ (show . glFun $ gl))
  mapM_ (putStrLn . empName) (take 10 . glGuests $ gl)
