{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.Language.Closure{-  
                            ( 
                              (.:)
                            , Clos
                            , declare
                            , record
                            , enum
                            , deriveEnum
                            , ClosureDescription
                            , ClosureDescriptable( .. )
                            , renderClosureEnvironment
                            ) -}where

import Control.Applicative( (<$>), (<*>), pure )
import Control.Monad.State( State, get, put, execState )
import qualified Data.Set as S
import Data.Monoid( Monoid, mappend, mconcat )
import Data.List ( intersperse )
import qualified Data.Text as T
import Text.Printf( printf )
{-import Data.Map  ( Map() )-}
import Data.Aeson( ToJSON(..), Value, (.=), object )

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

declare :: (ClosureDescriptable a kind) => a -> Clos ()
declare element = do
    rendered <- renderDeclaration element
    ctxt <- get
    put $ ctxt {
        definitions = S.insert (typename element) $ definitions ctxt,
        declarationList = declarationList ctxt ++ [rendered]
    }

renderDeclaration :: (ClosureDescriptable a kind) => a -> Clos String
renderDeclaration el = do
    t <- renderType el
    let name = typename el
    case toClosureDesc el of
      (ClosureEnum _ _ _ _) ->
            return $ printf "/** @enum {%s} */\nvar %s;\n\n" t name
      _ -> return $  printf "/** @typedef (%s) */\nvar %s;\n\n" t name

renderClosureEnvironment :: Clos () -> String
renderClosureEnvironment declarations =
    concat . declarationList $ execState declarations emptyEnvironment

--------------------------------------------------
----          Main  Typeclass
--------------------------------------------------

class ClosureDescriptable a kind | a -> kind where
    -- | Name of the type, used to locate it.
    typename :: a -> String

    toClosureDesc :: a -> ClosureDescription a kind

    toValue :: a -> Value

--------------------------------------------------
----            Types
--------------------------------------------------
data Accessor a kind where
     Accessor :: (ClosureDescriptable b kind) => String -> (a -> b) -> Accessor a kind

data EnumContent a kind where
     EnumContent :: (ClosureDescriptable b kind) => (a -> b)
                 -> EnumContent a kind

-- | Token for values which can be serialized
data Serializable

-- | Toekn for the rest of values
data Typeable

data ClosureDescList elem kind where
    Nil     :: ClosureDescList elem a

    Cons    :: elem k -> ClosureDescList elem a
            -> ClosureDescList elem Typeable

    ConsSer :: elem Serializable -> ClosureDescList elem Serializable
            -> ClosureDescList elem Serializable

class DescImportable a kind where
    toKind :: [a kind] -> ClosureDescList a kind

instance DescImportable a Typeable where
    toKind [] = Nil
    toKind (x:xs) = Cons x $ toKind xs

instance DescImportable a Serializable where
    toKind [] = Nil
    toKind (x:xs) = ConsSer x $ toKind xs

mapList :: (forall subKind. a subKind -> b) -> ClosureDescList a kind -> [b]
mapList _ Nil = []
mapList f (Cons x xs) = f x : mapList f xs
mapList f (ConsSer x xs) = f x : mapList f xs

mapSerialazable :: (a Serializable -> b) -> ClosureDescList a Serializable
                -> [b]
mapSerialazable _ Nil = []
mapSerialazable f (ConsSer x xs) = f x : mapSerialazable f xs

data ClosureDescription a kind where
   ClosureEnum      :: String -> [a] -> (a -> String) -> EnumContent a Serializable
                    -> ClosureDescription a Serializable

   ClosureRecord    :: String -> ClosureDescList (Accessor a) k
                    -> ClosureDescription a k

   ClosureType      :: String -> ClosureDescription a Typeable

   ClosureVal       :: String -> ClosureDescription a Serializable

   ClosureArray     :: String
                    -> ClosureDescription el kind
                    -> ClosureDescription a kind

   ClosureFunction  :: String
                    -> ClosureDescription el1 kind1
                    -> ClosureDescription el2 kind2
                    -> ClosureDescription a Typeable

   ClosureNil       :: ClosureDescription a Typeable

   ClosureTuple     :: ClosureDescription b kind1
                    -> ClosureDescription c kind2
                    -> ClosureDescription a Typeable

   ClosureTupleSub  :: ClosureDescription b kind1
                    -> ClosureDescription c kind2
                    -> ClosureDescription a Typeable

descriptionName :: ClosureDescription a k -> String
descriptionName (ClosureEnum  n _ _ _) = n
descriptionName (ClosureRecord n _) = n
descriptionName (ClosureType n) = n
descriptionName (ClosureVal n) = n
descriptionName (ClosureArray n _) = n
descriptionName (ClosureFunction n _ _) = n
descriptionName ClosureNil = ""
descriptionName (ClosureTuple _ _) = ""
descriptionName (ClosureTupleSub _ _) = ""

renderType :: (ClosureDescriptable a kind) => a -> Clos String
renderType element = do
  alreadyRendered <- isTypeRendered $ typename element
  if alreadyRendered
    then pure $ typename element
    else aux $ toClosureDesc element

   where renderSub :: ClosureDescription e k -> Clos String
         renderSub def = do
            let name = descriptionName def
            alreadyRendered <- isTypeRendered name
            if alreadyRendered
                then pure name
                else aux def


         aux :: forall a kind . ClosureDescription a kind -> Clos String
         aux ClosureNil      = pure ""
         aux (ClosureType s) = pure s
         aux (ClosureVal s) = pure s
         aux (ClosureTuple l r) = sepIt <$> renderSub l <*> renderSub r
            where sepIt a "" = a
                  sepIt a b  = "(" ++ a ++ ", " ++ b ++ ")"

         aux (ClosureTupleSub l r) = sepIt <$> renderSub l <*> renderSub r
            where sepIt a "" = a
                  sepIt a b  = a ++ ", " ++ b

         aux (ClosureFunction _ args rez) = 
            printf "function (%s) : %s" <$> renderSub args <*> renderSub rez

         aux (ClosureArray _ t) =
             surround "Array.<" ">" <$> renderSub t

         aux (ClosureEnum name _lst _f (EnumContent renderer)) =
             renderType (renderer undefined)

         aux (ClosureRecord _ lst) =
             surround "{ " " }" . commaSep <$> sequence (mapList subber lst)
                where subber :: (Accessor a aKind) -> Clos String
                      subber (Accessor s f) = printf "%s: %s" s
                                           <$> renderSub t
                        where e = f (undefined :: a)
                              t = toClosureDesc e


--------------------------------------------------
----            User chrome
--------------------------------------------------
    --
record :: forall a kind.
          (ClosureDescriptable a kind,
           DescImportable (Accessor a) kind) => [Accessor a kind]
       -> ClosureDescription a kind
record = ClosureRecord name . toKind
    where name = typename (undefined :: a)

(.:) :: (ClosureDescriptable b kind) => String -> (a -> b)
     -> Accessor a kind
(.:) = Accessor

commaSep :: [String] -> String
commaSep = mconcat . intersperse ",\n                "

surround :: (Monoid a) => a -> a -> a -> a
surround prev after v = prev `mappend` v `mappend` after

--------------------------------------------------
----            Here lie Serialization magic
--------------------------------------------------
instance (ClosureDescriptable a Serializable) => ToJSON a where
    toJSON a = serialize (toClosureDesc a) a

defaultSerializer :: (ClosureDescriptable a Serializable) => a -> Value
defaultSerializer v = serialize (toClosureDesc v) v

serialize :: (ClosureDescriptable a Serializable)
          => ClosureDescription a Serializable -> a -> Value
serialize (ClosureVal _) v = toValue v
serialize (ClosureEnum _ _ renderer _) v = toJSON $ renderer v
serialize (ClosureArray _ _)  arr = toValue arr
serialize (ClosureRecord _ lst) v = object $ mapSerialazable toVal lst
    where toVal (Accessor n f) = (T.pack n) .= toValue (f v)

enum :: (ClosureDescriptable a k,
         ClosureDescriptable b Serializable)
     => [a] -> (a -> String) -> (a -> b) -> ClosureDescription a Serializable
enum l f = ClosureEnum name l f . EnumContent 
    where name = typename (head l)

deriveEnum :: (Show a, Enum a, ClosureDescriptable a k)
           => a -> ClosureDescription a Serializable
deriveEnum v = ClosureEnum name elemList show (EnumContent (show . fromEnum))
    where elemList = [toEnum 0 ..]
          name = typename v

--------------------------------------------------
----         Initial type instances
--------------------------------------------------
instance ClosureDescriptable Int Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Bool Serializable where
    typename _ = "boolean"
    toClosureDesc _ = ClosureVal "boolean"
    toValue = toJSON

instance ClosureDescriptable Char Serializable where
    typename  _ = "string"
    toClosureDesc _ = ClosureVal "string"
    toValue = toJSON

instance ClosureDescriptable String Serializable where
    typename  _ = "string"
    toClosureDesc _ = ClosureVal "string"
    toValue = toJSON

instance (ClosureDescriptable a kind,
          ToJSON a) => ClosureDescriptable [a] kind where
    typename _ = "Array"
    toValue = toJSON
    toClosureDesc _ = ClosureArray ""
                    $ toClosureDesc (undefined :: a)

instance ( ClosureDescriptable a k1
         , ClosureDescriptable b k2 ) =>
         ClosureDescriptable (a,b) Typeable where
    typename _ = "tuple"
    toValue = error "Error tuples cannot be serialized"
    toClosureDesc tu = ClosureTuple d1 d2
        where d1 = toClosureDesc $ fst tu
              d2 = toClosureDesc $ snd tu

instance ( ClosureDescriptable a k1
         , ClosureDescriptable b k2
         , ClosureDescriptable c k3) =>
         ClosureDescriptable (a, b, c) Typeable where
    typename _ = "tuple"
    toValue = error "Error tuples cannot be serialized"
    -- Don't pattern match, argument can be undefined
    toClosureDesc tu =
        ClosureTuple (n1 tu) $ ClosureTupleSub (n2 tu) (n3 tu)
            where n1 ~(a, _, _) = toClosureDesc a
                  n2 ~(_, a, _) = toClosureDesc a
                  n3 ~(_, _, a) = toClosureDesc a

instance ( ClosureDescriptable a k1
         , ClosureDescriptable b k2) =>
         ClosureDescriptable ((->) a b) Typeable where
    typename _ = "function"
    toValue = error "Error functions cannot be serialized"
    toClosureDesc f = ClosureFunction "" (toClosureDesc a) (toClosureDesc b)
        where a = undefined :: a
              b = f a :: b

{-instance (ClosureDescriptable a) => ClosureDescriptable (Map String a) where-}
    {-typename _ = "Object"-}
    {-toClosureDesc _ = "Object.<string, " ++ sub ++ ">"-}
        {-where sub = toClosureDesc (undefined :: a)-}


