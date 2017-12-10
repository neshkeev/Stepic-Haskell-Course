import Text.Parsec

getList :: Parsec String u [String]
getList = many ((:) <$> optional (char ';') *> many1 alphaNum)
