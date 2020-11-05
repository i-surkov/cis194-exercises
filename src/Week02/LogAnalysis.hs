module Week02.LogAnalysis
  ( parseMessage,
    parse,
    insert,
    build,
    inOrder,
    whatWentWrong,
    module Week02.Log,
  )
where

import Week02.Log
  ( LogMessage (..),
    MessageTree (..),
    MessageType (..),
    TimeStamp,
    testParse,
    testWhatWentWrong,
  )

-- | Parse a log message into predefined format
parseMessage :: String -> LogMessage
parseMessage str = case words str of
  ("I" : t : msg) -> LogMessage Info (read t) (unwords msg)
  ("W" : t : msg) -> LogMessage Warning (read t) (unwords msg)
  ("E" : c : t : msg) -> LogMessage (Error $ read c) (read t) (unwords msg)
  _ -> Unknown str

-- | Parse log messages, devided by new lines
parse :: String -> [LogMessage]
parse str = [parseMessage message | message <- lines str]

-- | Insert a log message into sorted binary tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert _ tree@(Node _ (Unknown _) _) = tree -- this is just to be exhaustive
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ time _) (Node left tmsg@(LogMessage _ ttime _) right)
  | time < ttime = Node (insert msg left) tmsg right
  | otherwise = Node left tmsg (insert msg right)

-- | Build a sorted binary tree from a list of log messages.
-- Starts from the head of the list, just to satisfy a test case
build :: [LogMessage] -> MessageTree
build [] = Leaf
build list = last list `insert` build (init list)

-- | Assembled ordered list of log messages from a tree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- | Take an unsorted list of log messages and return sorted list of error
-- messages with severity level > 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
  [msg | LogMessage (Error c) _ msg <- inOrder (build messages), c > 50]
