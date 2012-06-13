import Text.Language.Closure

data TRecord = TRecord
    { aField :: Int
    , bField :: String
    , dabada :: String
    , axx :: [String]
    , bidule :: [[Bool]]
    , crap :: [DiffCrap]
    }

data TFunctions = TFunctions
    { simpleFunc :: Int -> String
    , complax    :: (Int, String) -> TRecord
    , yoDawg     :: ((Int, String, Bool) -> TRecord) -> Bool
    }

instance ClosureDescriptable TFunctions where
    typename _ = "tfunctions"
    toClosureDesc _ =
        record [ "simple"  .: simpleFunc
               , "complex" .: complax
               , "yodog"   .: yoDawg
               ]
    
instance ClosureDescriptable TRecord where
    typename _ = "trecord"
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

instance ClosureDescriptable DiffCrap where
    typename _ = "diffcrap"
    toClosureDesc _ = deriveEnum undefined
      {-enum [toEnum 0..] show assoc-}
        {-where assoc DiffPlouch = "+"-}
              {-assoc DiffDelche = "-"-}
              {-assoc DiffMouelshe = "~"-}

typeDecl :: Clos ()
typeDecl = do
    declare (undefined :: DiffCrap)
    declare (undefined :: TRecord)
    declare (undefined :: TFunctions)

main :: IO ()
main = do
    putStrLn $ renderClosureEnvironment typeDecl

