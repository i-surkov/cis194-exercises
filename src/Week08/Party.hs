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

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (fun + empFun emp)

-- See src/Week08/Employee.hs to implement
-- the monoid instance for GuestList.
-- Avoids orphans.

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--------------------------- Exercise 2

-- foldTree is defined in Data.Tree, go read it if you like

--------------------------- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results =
  (glCons boss . mconcat . map snd $ results, mconcat . map fst $ results)

--------------------------- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun tree =
  let (first, second) = foldTree nextLevel tree
   in max first second

--------------------------- Exercise 5

main :: IO ()
main = do
  content <- readFile "./resources/Week08/company.txt"
  let gl = maxFun . read $ content
  putStrLn ("Total fun: " ++ (show . glFun $ gl))
  mapM_ (putStrLn . empName) (take 10 . glGuests $ gl)
