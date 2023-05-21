{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.DAO where

import CSDC.Action
import CSDC.Image
import CSDC.Mail qualified as Mail
import CSDC.Mail.Templates qualified as Mail.Templates
import CSDC.MajorityJudgement qualified as MajorityJudgement
import CSDC.Prelude
import CSDC.SQL.Persons qualified as SQL.Persons
import Control.Monad (forM_)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum (..))
import Data.Password.Bcrypt (hashPassword, mkPassword)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath

createUser :: NewUser -> Action user (Id Person)
createUser newUser@(NewUser {..}) = do
  passwordHash <- hashPassword $ mkPassword password
  let person =
        NewPerson
          { name = name,
            email = email,
            password = passwordHash,
            description = "",
            image = ""
          }
  pid <- insertPerson person

  pure pid

getUser :: ActionAuth (Id Person)
getUser = asks (.user)
