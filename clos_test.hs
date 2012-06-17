{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
import Text.Language.Closure

import Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson( ToJSON(..), encode )

data TRecord = TRecord
    { aField :: Int
    , bField :: String
    , dabada :: String
    , axx :: [String]
    , bidule :: [[Bool]]
    , crap :: [DiffCrap]
    }

defaultTRecord :: TRecord
defaultTRecord = TRecord
    { aField = 42
    , bField = "bField"
    , dabada = "dabada"
    , axx = ["list", "of", "char"]
    , bidule = [[False,True], [True, False], [False] ]
    , crap = [DiffMouelshe, DiffPlouch]
    }

defaultTFunctions :: TFunctions
defaultTFunctions = TFunctions
    { simpleFunc = \_ -> "ok"
    , nestedFun = \_ -> True
    , complax = \_ -> defaultTRecord
    , yoDawg = \_ -> False
    }

data TFunctions = TFunctions
    { simpleFunc :: Int -> String
    , nestedFun  :: (String -> Int) -> Bool
    , complax    :: (Int, String) -> TRecord
    , yoDawg     :: ((Int, String, Bool) -> TRecord) -> Bool
    }

instance ClosureDescriptable TFunctions Typeable where
    typename _ = "tfunctions"
    toValue = undefined
    toClosureDesc _ =
        record [ "simple"  .: simpleFunc
               , "nested"  .: nestedFun
               , "complex" .: complax
               , "yodog"   .: yoDawg
               ]
    
instance ClosureDescriptable TRecord Serializable where
    typename _ = "trecord"
    toValue = defaultSerializer
    toClosureDesc _ =
        record [ "a" .: aField
               , "b" .: bField
               , "oki" .: dabada
               , "moktar" .: axx
               , "mwep" .: bidule
               , "ref" .: crap
               ]

data DiffCrap = DiffPlouch
              | DiffDelche
              | DiffMouelshe
              deriving (Enum, Show)

typeDecl :: ClosTypingEnvironment ()
typeDecl = do
    declare (undefined :: DiffCrap)
    declare (undefined :: TRecord)
    declare (undefined :: TFunctions)

instance ClosureDescriptable DiffCrap Serializable where
    typename _ = "diffcrap"
    toClosureDesc _ = deriveEnum undefined
      {-enum [toEnum 0..] show assoc-}
        {-where assoc DiffPlouch = "+"-}
              {-assoc DiffDelche = "-"-}
              {-assoc DiffMouelshe = "~"-}


main :: IO ()
main = do
    putStrLn $ renderClosureEnvironment typeDecl
    putStrLn . T.unpack . E.decodeUtf8  . B.concat . BL.toChunks . encode
             $ toJSON defaultTRecord

    {-putStrLn . T.unpack . E.decodeUtf8  . B.concat . BL.toChunks . encode-}
             {-$ toJSON defaultTFunctions-}

