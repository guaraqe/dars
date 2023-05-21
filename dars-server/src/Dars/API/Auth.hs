{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dars.API.Auth
  ( API,
    serveAPI,
    Settings (..),
    makeSettings,
    makeContext,
    contextProxy,
  )
where

import Dars.API.Common qualified as Common
import Dars.Action hiding (Context)
-- import Dars.Common (createAPIUser)
import Dars.Prelude hiding (Post)
import Data.Password.Bcrypt (PasswordCheck (..), checkPassword, mkPassword)
import Servant hiding (Server, Unauthorized, throwError)
import Servant.Auth.Server
  ( Auth,
    AuthResult (..),
    Cookie,
    CookieSettings (..),
    FromJWT,
    JWTSettings,
    SetCookie,
    ToJWT,
    acceptLogin,
    clearSession,
    defaultCookieSettings,
    defaultJWTSettings,
    generateKey,
    readKey,
    writeKey,
  )
import Servant.Server.Generic (AsServerT)
import System.Directory (doesFileExist)

--------------------------------------------------------------------------------
-- APIUser

newtype APIUser = APIUser {getAPIUser :: Id User}
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

instance ToJWT APIUser

instance FromJWT APIUser

--------------------------------------------------------------------------------
-- Login

data Login = Login {email :: !Text, password :: !Text}
  deriving (Eq, Show, Generic)

instance ToJSON Login

instance FromJSON Login

--------------------------------------------------------------------------------
-- Signin API

type CookieHeaders =
  Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]

data SigninAPI mode = SigninAPI
  { signin :: mode :- ReqBody '[JSON] Login :> Verb 'POST 204 '[JSON] (CookieHeaders NoContent)
  }
  deriving (Generic)

serveSigninAPI :: Settings -> Server SigninAPI
serveSigninAPI settings =
  SigninAPI
    { signin = authenticate settings
    }

authenticate :: Settings -> Login -> Action () (CookieHeaders NoContent)
authenticate (Settings cookieSettings jwtSettings) (Login email password) =
  runQuery SQL.Persons.check email >>= \case
    Nothing ->
      throw Unauthorized
    Just (personId, passwordHash) -> do
      case checkPassword (mkPassword password) passwordHash of
        PasswordCheckFail ->
          throw Unauthorized
        PasswordCheckSuccess -> do
          mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (APIUser personId)
          case mApplyCookies of
            Nothing ->
              throw Unauthorized
            Just applyCookies ->
              return $ applyCookies NoContent

--------------------------------------------------------------------------------
-- Signup API

data SignupAPI mode = SignupAPI
  { signup :: mode :- ReqBody '[JSON] NewUser :> Post '[JSON] (Id User)
  }
  deriving (Generic)

serveSignupAPI :: Server SignupAPI
serveSignupAPI =
  SignupAPI
    { signup = undefined -- createAPIUser
    }

--------------------------------------------------------------------------------
-- Signout API

data SignoutAPI mode = SignoutAPI
  { signout :: mode :- Get '[JSON] (CookieHeaders Text)
  }
  deriving (Generic)

serveSignoutAPI :: Monad m => Settings -> SignoutAPI (AsServerT m)
serveSignoutAPI (Settings {..}) =
  SignoutAPI
    { signout = return $ clearSession settingsCookie ""
    }

--------------------------------------------------------------------------------
-- API with auth

serveAuthAPI :: AuthResult APIUser -> Server Common.NamedAPI
serveAuthAPI (Authenticated (APIUser personId)) =
  hoistServer (Proxy @Common.API) (withPerson personId) Common.serveAPI
serveAuthAPI _ = throwUnauthorized

data API mode = API
  { daoAPI :: mode :- "api" :> Auth '[Cookie] APIUser :> Common.API,
    signinAPI :: mode :- "signin" :> NamedRoutes SigninAPI,
    signupAPI :: mode :- "signup" :> NamedRoutes SignupAPI,
    signoutAPI :: mode :- "signout" :> NamedRoutes SignoutAPI
  }
  deriving (Generic)

data Settings = Settings
  { settingsCookie :: CookieSettings,
    settingsJWT :: JWTSettings
  }

makeSettings :: Maybe FilePath -> IO Settings
makeSettings mpath = do
  key <- case mpath of
    Nothing -> generateKey
    Just path -> do
      doesFileExist path >>= \case
        False ->
          writeKey path
        True ->
          pure ()
      readKey path
  pure
    Settings
      { settingsCookie = defaultCookieSettings {cookieXsrfSetting = Nothing},
        settingsJWT = defaultJWTSettings key
      }

serveAPI :: Settings -> Server API
serveAPI settings =
  API
    { daoAPI = serveAuthAPI,
      signinAPI = serveSigninAPI settings,
      signupAPI = serveSignupAPI,
      signoutAPI = serveSignoutAPI settings
    }

contextProxy :: Proxy '[CookieSettings, JWTSettings]
contextProxy = Proxy

makeContext :: Settings -> Context '[CookieSettings, JWTSettings]
makeContext Settings {..} =
  settingsCookie :. settingsJWT :. EmptyContext

--------------------------------------------------------------------------------
-- Throw All

class ThrowUnauthorized a where
  throwUnauthorized :: a

instance
  (ThrowUnauthorized a, ThrowUnauthorized b) =>
  ThrowUnauthorized (a :<|> b)
  where
  throwUnauthorized = throwUnauthorized :<|> throwUnauthorized

instance
  {-# OVERLAPPING #-}
  ThrowUnauthorized b =>
  ThrowUnauthorized (a -> b)
  where
  throwUnauthorized = const throwUnauthorized

instance {-# OVERLAPPABLE #-} ThrowUnauthorized (Action user a) where
  throwUnauthorized = throw Unauthorized

instance
  {-# OVERLAPPABLE #-}
  ( ThrowUnauthorized (ToServant routes mode),
    GenericServant routes mode
  ) =>
  ThrowUnauthorized (routes mode)
  where
  throwUnauthorized = fromServant throwUnauthorized
