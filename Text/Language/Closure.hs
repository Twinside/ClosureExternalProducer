{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
-- I know, but I need it
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Automatically generate Google closure extern file
--
-- How to create an extern :
--
--  * create ClosureDescriptable instance for all your types
--
--  * Create a typing environment
--
--  * Render the typing environment
--
-- here is a little example for the typing environment creation
-- and rendering
--
-- @
-- closureDeclarations :: String
-- closureDeclarations = renderClosureEnvironment $ do
--      declare (undefined :: AnEnum)
--      declare (undefined :: DiffType)
--      declare (undefined :: RecordTest)
-- @
--
module Text.Language.Closure( 
                            -- * Closure type declarations
                              declare
                            , renderClosureEnvironment

                            , defaultSerializer

                            -- ** Record declaration
                            , Accessor
                            , record
                            , (.:)

                            -- ** Enum declaration
                            , enum
                            , deriveEnum

                            -- * Type definitions
                            , ClosureDescriptable( .. )
                            , ClosTypingEnvironment()
                            , ClosureDescription

                            -- * Type tokens
                            , Typeable
                            , Serializable
                            ) where

import Control.Applicative( (<$>), (<*>), pure )
import Control.Monad.State( State, get, put, execState )
import qualified Data.Set as S
import Data.Monoid( Monoid, mappend, mconcat )
import Data.List ( intersperse )
import Data.Word( Word, Word16, Word32, Word64 )
import Data.Int( Int16, Int32, Int64 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Text.Printf( printf )
import qualified Data.Map as M
import Data.Aeson( ToJSON(..), Value, (.=), object, encode )

--------------------------------------------------
----            Typing environment
--------------------------------------------------

-- | Typing environment, keep track of already declared
-- data types and their rendered representations
data ClosureEnvironment = ClosureEnvironment 
    { -- | Already defined data types
      definitions       :: S.Set String

      -- | Rendered type declaration.
    , declarationList   :: [String]
    }

-- | Default initial environment : everything is empty
emptyEnvironment :: ClosureEnvironment
emptyEnvironment = ClosureEnvironment
    { definitions = S.empty
    , declarationList = []
    }

-- | Typing environment used to declare all the closure
-- types which will be printed in an extern files.
type ClosTypingEnvironment a = State ClosureEnvironment a

-- | Tell if we already have rendered a type.
isTypeRendered :: String -> ClosTypingEnvironment Bool
isTypeRendered name = S.member name . definitions <$> get

-- | Function used to declare a typedef in the typing environment.
declare :: (ClosureDescriptable a kind) => a -> ClosTypingEnvironment ()
declare element = do
    rendered <- renderDeclaration element
    ctxt <- get
    put $ ctxt {
        definitions = S.insert (typename element) $ definitions ctxt,
        declarationList = declarationList ctxt ++ [rendered]
    }

valToString :: Value -> String
valToString = T.unpack . E.decodeUtf8 . B.concat . LB.toChunks . encode 

-- | Render a type declaration, in a closure like fashion.
renderDeclaration :: (ClosureDescriptable a kind) => a -> ClosTypingEnvironment String
renderDeclaration el = do
    t <- renderType el
    let name = typename el
    case toClosureDesc el of
      (ClosureEnum _ elems render (EnumContent toContent)) ->
         return $ printf "/** @enum {%s} */\nvar %s = {\n%s\n};\n\n" t name content
            where content = concat $ intersperse ",\n"
                        ["   " ++ render e ++ ": " ++ enumVal
                                    | e <- elems
                                    , let enumVal  = valToString . toValue $ toContent e]

      _ -> return $  printf "/** @typedef (%s) */\nvar %s;\n\n" t name

-- | Render a typing environment to a Google Closure declaration.
renderClosureEnvironment :: ClosTypingEnvironment () -> String
renderClosureEnvironment declarations =
    concat . declarationList $ execState declarations emptyEnvironment

--------------------------------------------------
----          Main  Typeclass
--------------------------------------------------

-- | Typeclass permitting accessing the type description
-- of a type and some serialization if kind is Serializable
-- Only two kind of declaration are allowed :
--
-- @
-- instance ClosureDescriptable a Serializable
-- instance ClosureDescriptable a Typeable
-- @
--
class ClosureDescriptable a kind | a -> kind where
    -- | Name of the type, used to locate it.
    typename :: a -> String

    -- | Extract the type description of type.
    toClosureDesc :: a -> ClosureDescription a kind

    -- | Synonym to the toJSON function.
    toValue :: a -> Value
    toValue = error "Not defined ClosureDescriptable::toValue method"

--------------------------------------------------
----            Types
--------------------------------------------------

-- | Accessor for a record element, The only way to create Accessor
-- is through the `.:` operator.
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

-- | Represent the google closure type langage.
-- The first type parameter represent the Haskell type
-- being described, and the second one represent one
-- of the type token `Typeable` or `Serializable` indicating
-- if we can serialize the description or just output a
-- type representation.
data ClosureDescription a kind where
   ClosureEnum      :: String -> [a] -> (a -> String) -> EnumContent a Serializable
                    -> ClosureDescription a Serializable

   ClosureRecord    :: String -> ClosureDescList (Accessor a) k
                    -> ClosureDescription a k

   ClosureType      :: String -> ClosureDescription a Typeable

   ClosureVal       :: String -> ClosureDescription a Serializable

   ClosureDowncast  :: ClosureDescription a k
                    -> ClosureDescription a Typeable

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
   
   ClosureAssoc :: String
                -> ClosureDescription c kind
                -> ClosureDescription a kind

   ClosureObject    :: String
                    -> ClosureDescription b k1
                    -> ClosureDescription c k2
                    -> ClosureDescription a Typeable

descriptionName :: ClosureDescription a k -> String
descriptionName (ClosureAssoc n _) = n
descriptionName (ClosureDowncast a) = descriptionName a
descriptionName (ClosureObject n _ _) = n
descriptionName (ClosureEnum  n _ _ _) = n
descriptionName (ClosureRecord n _) = n
descriptionName (ClosureType n) = n
descriptionName (ClosureVal n) = n
descriptionName (ClosureArray n _) = n
descriptionName (ClosureFunction n _ _) = n
descriptionName ClosureNil = ""
descriptionName (ClosureTuple _ _) = ""
descriptionName (ClosureTupleSub _ _) = ""

renderType :: (ClosureDescriptable a kind) => a -> ClosTypingEnvironment String
renderType element = do
  alreadyRendered <- isTypeRendered $ typename element
  if alreadyRendered
    then pure $ typename element
    else aux $ toClosureDesc element

   where renderSub :: ClosureDescription e k -> ClosTypingEnvironment String
         renderSub def = do
            let name = descriptionName def
            alreadyRendered <- isTypeRendered name
            if alreadyRendered
                then pure name
                else aux def


         aux :: forall a kind . ClosureDescription a kind -> ClosTypingEnvironment String
         aux ClosureNil      = pure ""
         aux (ClosureDowncast a) = aux a
         aux (ClosureType s) = pure s
         aux (ClosureVal s) = pure s
         aux (ClosureObject _ key el) =
             printf "Object.<%s, %s>" <$> renderSub key <*> renderSub el

         aux (ClosureAssoc _ el) =
             printf "Object.<string, %s>" <$> renderSub el

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

         aux (ClosureEnum _name _lst _f (EnumContent renderer)) =
             renderType (renderer undefined)

         aux (ClosureRecord _ lst) =
             surround "{ " " }" . commaSep <$> sequence (mapList subber lst)
                where subber :: (Accessor a aKind) -> ClosTypingEnvironment String
                      subber (Accessor s f) = printf "%s: %s" s
                                           <$> renderSub t
                        where e = f (undefined :: a)
                              t = toClosureDesc e


--------------------------------------------------
----            User chrome
--------------------------------------------------

-- | This function should be used in an instance declaration
-- to declare a google closure record.
--
-- Here a little use example
--
-- @
-- data RecordTest = RecordTest
--      { aField    :: String
--      , otherFild :: Int
--      , aList     :: [Int]
--      }
--
-- instance ClosureDescriptable RecordTest Serializable where
--    typename _ = \"recordtest\"
--    toValue = defaultSerializer
--    toClosureDesc _ =
--          record [ \"aField\"        .: aField
--                 , \"anotherField\"  .: anotherField
--                 , \"aListOfNumber\" .: aList
--                 ]
-- @
--
record :: forall a kind.
          (ClosureDescriptable a kind,
           DescImportable (Accessor a) kind)
       => [Accessor a kind]             -- ^ List of accessor, created via `.:`
       -> ClosureDescription a kind     -- ^ Record description
record = ClosureRecord name . toKind
    where name = typename (undefined :: a)

-- | Create a record accessor, see `record`
(.:) :: (ClosureDescriptable b kind)
     => String      -- ^ Record entry name
     -> (a -> b)    -- ^ Function used to access the element.
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

-- | Default implementation to use for the `toValue` method.
defaultSerializer :: (ClosureDescriptable a Serializable) => a -> Value
defaultSerializer v = serialize (toClosureDesc v) v

serialize :: (ClosureDescriptable a Serializable)
          => ClosureDescription a Serializable -> a -> Value
serialize (ClosureVal _) v = toValue v
serialize (ClosureAssoc _ _) v = toValue v
serialize (ClosureEnum _ _ _ (EnumContent f)) v = serialize (toClosureDesc value) value
    where value = f v
serialize (ClosureArray _ _)  arr = toValue arr
serialize (ClosureRecord _ lst) v = object $ mapSerialazable toVal lst
    where toVal (Accessor n f) = (T.pack n) .= toValue (f v)

-- | Create a custom Closure enum.
-- 
-- Usage example :
--
-- @
--  data DiffType = DiffAdd
--                | DiffDel
--                | DiffSame
--                deriving (Eq, Show)
--
--  instance ClosureDescriptable DiffType Serializable where
--     typename _ = \"difftype\"
--     toValue = defaultSerializer
--     toClosureDesc _ = enum [toEnum 0 ..] show assoc
--        where assoc DiffAdd = \"+\"
--              assoc DiffDel = \"-\"
--              assoc DiffSame = \"=\"
-- @
--
enum :: (ClosureDescriptable a k,
         ClosureDescriptable b Serializable)
     => [a]             -- ^ List of all the elements present in the enumaration
     -> (a -> String)   -- ^ A function used to name the elements of the enumeration
     -> (a -> b)        -- ^ A function used to convert an enum element to its serialized representation
     -> ClosureDescription a Serializable
enum l f = ClosureEnum name l f . EnumContent 
    where name = typename (head l)

-- | Helper function used to declare an enumeration inside
-- the closure typing environment.
--
-- Here is a little use example :
--
-- @
--  data AnEnum = AnEnumElem
--              | AnEnumOtherElem
--              | AnEnumSomething
--              | AnEnumElse
--              | AnEnumFinally
--              deriving (Eq, Enum, Show)
--
--  instance ClosureDescriptable AnEnum Serializable where
--      typename _ = \"anenum\"
--      toValue = defaultSerializer
--      toClosureDesc _ = deriveEnum undefined
-- @
--
deriveEnum :: (Show a, Enum a, ClosureDescriptable a k)
           => a         -- ^ a Dummy value of the type, unused
           -> ClosureDescription a Serializable
deriveEnum v = ClosureEnum name elemList show (EnumContent fromEnum)
    where elemList = [toEnum 0 ..]
          name = typename v

--------------------------------------------------
----         "base" type instances
--------------------------------------------------
instance ClosureDescriptable Int Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Int16 Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Int32 Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Int64 Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Word Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Word16 Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Word32 Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Word64 Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Float Serializable where
    typename  _ = "number"
    toClosureDesc _ = ClosureVal "number"
    toValue = toJSON

instance ClosureDescriptable Double Serializable where
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

instance ClosureDescriptable T.Text Serializable where
    typename _ ="string"
    toClosureDesc _ = ClosureVal "string"
    toValue = toJSON

instance ClosureDescriptable B.ByteString Serializable where
    typename _ ="string"
    toClosureDesc _ = ClosureVal "string"
    toValue = toJSON

--------------------------------------------------
----            Array/String instances
--------------------------------------------------
instance ClosureDescriptable String Serializable where
    typename  _ = "string"
    toClosureDesc _ = ClosureVal "string"
    toValue = toJSON

class (ClosureDescriptable a k) => ArraySerializable a k where
    listSerialize :: [a] -> Value

instance (ClosureDescriptable a Serializable)
         => ArraySerializable a Serializable where
    listSerialize = toJSON . map s
        where s a = serialize (toClosureDesc a) a

instance (ClosureDescriptable a Typeable)
         => ArraySerializable a Typeable where
    listSerialize = error "Cannot serialize value from typeable"

instance (ArraySerializable a kind, ClosureDescriptable a kind)
       => ClosureDescriptable [a] kind where
    typename _ = "Array"
    -- It's, let's aknoweledge it, ugly. But the type system
    -- ensure us that we won't be able to construct a value
    -- not serializable.
    toValue = listSerialize

    toClosureDesc ~(v:_) = ClosureArray "" $ toClosureDesc v


--------------------------------------------------
----            "Functions" instance
--------------------------------------------------
instance ( ClosureDescriptable a k1
         , ClosureDescriptable b k2 ) =>
         ClosureDescriptable (a,b) Typeable where
    typename _ = "tuple"
    toValue = error "Error tuples cannot be serialized"
    toClosureDesc ~(d1, d2) =
        ClosureTuple (toClosureDesc d1) (toClosureDesc d2)

instance ( ClosureDescriptable a k1
         , ClosureDescriptable b k2
         , ClosureDescriptable c k3) =>
         ClosureDescriptable (a, b, c) Typeable where
    typename _ = "tuple"
    toValue = error "Error tuples cannot be serialized"
    toClosureDesc ~(a, b, c) =
        ClosureTuple (toClosureDesc a)
                   $ ClosureTupleSub (toClosureDesc b)
                                     (toClosureDesc c)

instance ( ClosureDescriptable a k1
         , ClosureDescriptable b k2) =>
         ClosureDescriptable ((->) a b) Typeable where
    typename _ = "function"
    toValue = error "Error functions cannot be serialized"
    toClosureDesc f = ClosureFunction "" (toClosureDesc a) (toClosureDesc b)
        where a = undefined :: a
              b = f a :: b

--------------------------------------------------
----            "Objects" instance
--------------------------------------------------
-- SOme overlap probelms can arrize with the Typeable constraint.
instance ( ClosureDescriptable elem Serializable )
        => ClosureDescriptable (M.Map String elem) Serializable where
    typename _ = "Object"
    toClosureDesc _ = ClosureAssoc "" $ toClosureDesc (undefined :: elem)
    toValue v = object [T.pack k .= toValue e | (k, e) <- M.assocs v]

instance ( ClosureDescriptable elem Serializable )
        => ClosureDescriptable (M.Map T.Text elem) Serializable where
    typename _ = "Object"
    toClosureDesc _ = ClosureAssoc "" $ toClosureDesc (undefined :: elem)
    toValue v = object [k .= toValue e | (k, e) <- M.assocs v]

instance ( ClosureDescriptable elem Serializable )
        => ClosureDescriptable (M.Map B.ByteString elem) Serializable where
    typename _ = "Object"
    toClosureDesc _ = ClosureAssoc "" $ toClosureDesc (undefined :: elem)
    toValue v = object [conv k .= toValue e | (k, e) <- M.assocs v]
        where conv = E.decodeUtf8

instance ( ClosureDescriptable key k1
         , ClosureDescriptable elem k2
         , kind ~ Typeable
         )
        => ClosureDescriptable (M.Map key elem) kind where
    typename _ = "Object"
    toClosureDesc _ = ClosureObject "" keyType elemType
        where keyType = toClosureDesc (undefined :: key)
              elemType = toClosureDesc (undefined :: elem)

