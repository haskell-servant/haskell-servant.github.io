{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

data Book = Book
  { title  :: Text
  , author :: Text
  } deriving Generic

-- JSON instances
instance FromJSON Book
instance ToJSON Book

-- PostgreSQL instances
instance FromRow Book where
  fromRow = Book <$> field <*> field

instance ToRow Book where
  toRow book = [ toField (title book)
               , toField (author book)
               ]

             -- we explicitly say we expect a request body,
             -- of type Book
type BookApi = "books" :> ReqBody Book :> Post Book  -- POST /books
          :<|> "books" :> Get [Book]                 -- GET /books

-- Let's stick the API type again here:
type BookApi = "books" :> ReqBody Book :> Post Book  -- POST /books
          :<|> "books" :> Get [Book]                 -- GET /books

server :: Connection -> Server BookApi
server conn = postBook
         :<|> getBooks

  where -- the aforementioned 'ReqBody' automatically makes this handler
        -- receive a Book argument
        postBook book = liftIO $ execute conn "insert into books values (?, ?)" book >> return book
        getBooks      = liftIO $ query_ conn "select * from books"

bookApi :: Proxy BookApi
bookApi = Proxy

main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost user=bookstore dbname=bookstore"
  run 8080 (serve bookApi $ server conn)
