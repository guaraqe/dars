{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Dars.API
  ( API,
    serveAPI,
  )
where

import Dars.API.Auth qualified as Auth
import Dars.Action
import Dars.FileServer (serveSQLFileServer)
import Dars.SQL qualified as SQL
import GHC.Generics (Generic)
import Servant hiding (Server)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)

--------------------------------------------------------------------------------
-- API

data API mode = API
  { authAPI :: mode :- NamedRoutes Auth.API,
    filesAPI :: mode :- "files" :> Raw,
    rawAPI :: mode :- Raw
  }
  deriving (Generic)

serveAPI :: FilePath -> SQL.Context -> Auth.Settings -> Server API
serveAPI path ctx settings =
  API
    { authAPI = Auth.serveAPI settings,
      filesAPI = serveSQLFileServer ctx,
      rawAPI = serveDirectoryWith (options path)
    }

options :: FilePath -> StaticSettings
options path =
  let base = defaultWebAppSettings path

      indexRedirect old = \case
        [] -> old [unsafeToPiece "index.html"]
        pcs -> old pcs
   in base {ssLookupFile = indexRedirect (ssLookupFile base)}
