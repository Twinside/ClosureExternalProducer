{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverlappingInstances #-}
module Text.Language.Closure( (.:)
                            , Clos
                            , declare
                            , record
                            , enum
                            , deriveEnum
                            , ClosureDescription
                            , ClosureDescriptable( .. )
                            , renderClosureEnvironment
                            ) where

import Control.Applicative( (<$>), pure )
import Control.Monad.State( State, get, put, execState )
import qualified Data.Set as S
import Data.Monoid( Monoid, mappend, mconcat )
import Data.List ( intersperse )
import Data.Word ( Word, Word8, Word16, Word32, Word64 )
import Text.Printf( printf )
{-import Data.Map  ( Map() )-}
{-import qualified Data.Text as T-}
{-import Data.Aeson( ToJSON(..), Value, (.=), object )-}

--------------------------------------------------
----            Typing environment
--------------------------------------------------
data ClosureEnvironment = ClosureEnvironment 
    { definitions       :: S.Set String
    , declarationList   :: [String]
    }

emptyEnvironment :: ClosureEnvironment
emptyEnvironment = ClosureEnvironment
    { definitions = S.empty
    , declarationList = []
    }

type Clos a = State ClosureEnvironment a

isTypeRendered :: String -> Clos Bool
isTypeRendered name = S.member name . definitions <$> get

declare :: (ClosureDescriptable a) => a -> Clos ()
declare element = do
    rendered <- renderDeclaration element
    ctxt <- get
    put $ ctxt {
        definitions = S.insert (typename element) $ definitions ctxt,
        declarationList = declarationList ctxt ++ [rendered]
    }

renderDeclaration :: (ClosureDescriptable a) => a -> Clos String
renderDeclaration el = do
    t <- renderType el
    let name = typename el
    case toClosureDesc el of
      (Enum _ _ _) ->
            return $ printf "/* @enum {%s} */\n var %s;\n\n" t name
      _ -> return $  printf "/* @typedef (%s) */\nvar %s;\n\n" t name

renderClosureEnvironment :: Clos () -> String
renderClosureEnvironment declarations =
    concat . declarationList $ execState declarations emptyEnvironment

renderType :: (ClosureDescriptable a) => a -> Clos String
renderType element = do
  alreadyRendered <- isTypeRendered $ typename element
  if alreadyRendered
    then pure $ typename element
    else aux $ toClosureDesc element

   where aux (ClosureType s) = pure $ s
         aux (ClosureTuple s) = concat
                              . intersperse ", " 
                             <$> sequence [renderType t | ClosureTypeable t <- s]

         aux (ClosureFunction (ClosureTypeable arg) (ClosureTypeable rez)) = do
             rArg <- renderType arg
             rRez <- renderType rez
             if null rRez
                then pure $ printf "function (%s)" rArg
                else pure $ printf "function (%s) : %s" rArg rRez

         aux (ClosureArray (ClosureTypeable a)) =do
             sub <- renderType a
             pure $ surround "Array.<" ">" sub

         aux (Enum _lst _f (EnumContent renderer)) = renderType (renderer undefined)

         aux (ClosureRecord lst) = do
             let subber (s, Renderable f) =
                        ((s ++ ": ") ++) <$> renderType (f undefined)
             surround "{ " " }" . commaSep <$> mapM subber lst

--------------------------------------------------
----            Types
--------------------------------------------------
data RecordAccessor a = forall b. (ClosureDescriptable b) => Renderable (a -> b)

data EnumContent a =
    forall b. (ClosureDescriptable b) => EnumContent (a -> b)

data ClosureTypeable = forall b. ClosureDescriptable b => ClosureTypeable b

data ClosureDescription a =
          Enum [a] (a -> String) (EnumContent a)
        | ClosureRecord [(String, RecordAccessor a)]
        | ClosureType String
        | ClosureArray ClosureTypeable
        | ClosureFunction ClosureTypeable ClosureTypeable
        | ClosureTuple [ClosureTypeable]

--------------------------------------------------
----          Main  Typeclass
--------------------------------------------------

-- | Minimal complete definition : `typename`
class ClosureDescriptable a where
    -- | Name of the type, used to locate it.
    typename :: a -> String

    -- | Return the signature used to represent it
    -- Default implementation : typename
    toClosureDesc :: a -> ClosureDescription a
    toClosureDesc a = ClosureType $ typename a

--------------------------------------------------
----            User chrome
--------------------------------------------------
(.:) :: (ClosureDescriptable b) => String -> (a -> b) -> (String, RecordAccessor a)
(.:) a f = (a, Renderable f)

record :: [(String, RecordAccessor a)] -> ClosureDescription a
record = ClosureRecord

enum :: (ClosureDescriptable b) => [a] -> (a -> String) -> (a -> b) -> ClosureDescription  a
enum l f = Enum l f . EnumContent 

deriveEnum :: (Show a, Enum a) => a -> ClosureDescription a
deriveEnum _ = Enum elemList show (EnumContent (show . fromEnum))
    where elemList = [toEnum 0 ..]

surround :: (Monoid a) => a -> a -> a -> a
surround prev after v = prev `mappend` v `mappend` after

commaSep :: [String] -> String
commaSep = mconcat . intersperse ", "

--------------------------------------------------
----         Initial type instances
--------------------------------------------------
instance ClosureDescriptable Int where
    typename _ = "number"

instance ClosureDescriptable Bool where
    typename _ = "boolean"

instance ClosureDescriptable Float where
    typename _ = "number"

instance ClosureDescriptable Double where
    typename _ = "number"

instance ClosureDescriptable Word where
    typename _ = "number"

instance ClosureDescriptable Word8 where
    typename _ = "number"

instance ClosureDescriptable Word16 where
    typename _ = "number"

instance ClosureDescriptable Word32 where
    typename _ = "number"

instance ClosureDescriptable Word64 where
    typename _ = "number"

instance ClosureDescriptable Char where
    typename _ = "char"

instance ClosureDescriptable String where
    typename _ = "string"

instance (ClosureDescriptable a) => ClosureDescriptable [a] where
    typename _ = "Array"
    toClosureDesc _ = ClosureArray $ ClosureTypeable (undefined :: a)

instance ( ClosureDescriptable a
         , ClosureDescriptable b ) =>
         ClosureDescriptable (a,b) where
    typename _ = "tuple"

    -- Don't pattern match, argument can be undefined
    toClosureDesc tu =
        ClosureTuple [ClosureTypeable $ fst tu, ClosureTypeable $ snd tu]

instance ( ClosureDescriptable a
         , ClosureDescriptable b
         , ClosureDescriptable c ) =>
         ClosureDescriptable (a,b,c) where
    typename _ = "tuple"

    -- Don't pattern match, argument can be undefined
    toClosureDesc tu =
        ClosureTuple [ ClosureTypeable $ n1 tu
                     , ClosureTypeable $ n2 tu
                     , ClosureTypeable $ n3 tu
                     ]
            where n1 (a, _, _) = a
                  n2 (_, a, _) = a
                  n3 (_, _, a) = a

instance ( ClosureDescriptable a
         , ClosureDescriptable b) =>
         ClosureDescriptable ((->) a b) where
    typename _ = "function"
    toClosureDesc f = ClosureFunction (ClosureTypeable a) (ClosureTypeable b)
        where a = undefined :: a
              b = f a :: b

{-instance (ClosureDescriptable a) => ClosureDescriptable (Map String a) where-}
    {-typename _ = "Object"-}
    {-toClosureDesc _ = "Object.<string, " ++ sub ++ ">"-}
        {-where sub = toClosureDesc (undefined :: a)-}

