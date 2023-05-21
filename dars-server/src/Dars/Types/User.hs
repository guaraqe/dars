{-# LANGUAGE OverloadedStrings #-}

module Dars.Types.User
  ( User (..),
  , NewUser (..),
  )
where

import Data.Text (Text)

data User = User
  { userId :: Id User
  , name :: Text
  }

data NewUser = NewUser
  { name :: Text
  }
