-- Author: Jekaterina Jevtejeva

data TTT aa
  = Node [aa] (TTT aa) (TTT aa) (TTT aa)
  | Empty
  deriving (Show)

mm :: (aa -> aa) -> TTT aa -> TTT aa
mm f (Node [] node1 node2 node3) = Node [] (mm f node1) (mm f node2) (mm f node3)
mm f (Node list node1 node2 node3) = Node (map f list) (mm f node1) (mm f node2) (mm f node3)
mm f Empty = Empty

a :: Integer -> Integer
a n = n * n

b :: Integer -> Integer
b n = mod n 5

c :: Integer -> Integer
c n = 88 - n

ff_a :: TTT Integer -> TTT Integer
ff_a tree = mm a tree

ff_b :: TTT Integer -> TTT Integer
ff_b tree = mm b tree

ff_c :: TTT Integer -> TTT Integer
ff_c tree = mm c tree

-- testTree:
--               [1,2,3]
--              /   |   \
--     [-5,-6,-7]   []  [11,12,13,-14]
--         |                  |
--     [8,9,10]              [15]
main :: IO ()
main = do
  let testTree =
        Node
          [1, 2, 3]
          (Node
              [-5, -6, -7]
              Empty
              (Node [8, 9, 10] Empty Empty Empty)
              Empty
          )
          (Node [] Empty Empty Empty)
          (Node
              [11, 12, 13, -14]
              (Node [15] Empty Empty Empty)
              Empty
              Empty
          )

  print testTree
  putStrLn "ff_a:"
  -- Expected:
  --               [1,4,9]
  --              /   |   \
  --     [25,36,49]   []  [121,144,169,196]
  --         |                  |
  --     [64,81,100]          [225]
  print (ff_a testTree)
  putStrLn "ff_b:"
  -- Expected
  --               [1,2,3]
  --              /   |   \
  --        [0,4,3]   []  [1,2,3,1]
  --           |              |
  --        [3,4,0]          [0]
  print (ff_b testTree)
  putStrLn "ff_c:"
  -- Expected:
  --              [87,86,85]
  --              /   |   \
  --     [93,94,95]   []  [77,76,75,102]
  --         |                  |
  --     [80,79,78]            [73]
  print (ff_c testTree)