-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Data
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell.
-- See <http://www.cs.vu.nl/boilerplate/>. This module provides
-- the 'Data' class with its primitives for generic programming, along
-- with instances for many datatypes. It corresponds to a merge between
-- the previous "Data.Generics.Basics" and almost all of 
-- "Data.Generics.Instances". The instances that are not present
-- in this module are available in "Data.Generics.Instances".
--
--
-----------------------------------------------------------------------------

module Data.Data (

        -- * Module Data.Typeable re-exported for convenience
        module Data.Typeable,

        -- * The Data class for processing constructor applications
        Data(
                gfoldl,         -- :: ... -> a -> c a
                gunfold,        -- :: ... -> Constr -> c a
                toConstr,       -- :: a -> Constr
                dataTypeOf,     -- :: a -> DataType
                dataCast1,      -- mediate types and unary type constructors
                dataCast2,      -- mediate types and binary type constructors
                -- Generic maps defined in terms of gfoldl 
                gmapT,
                gmapQ,
                gmapQl,
                gmapQr,
                gmapQi,
                gmapM,
                gmapMp,
                gmapMo
            ),

        -- * Datatype representations
        DataType,       -- abstract, instance of: Show
        -- ** Constructors
        mkDataType,     -- :: String   -> [Constr] -> DataType
        mkIntType,      -- :: String -> DataType
        mkFloatType,    -- :: String -> DataType
        mkStringType,   -- :: String -> DataType
        mkNorepType,    -- :: String -> DataType
        -- ** Observers
        dataTypeName,   -- :: DataType -> String
        DataRep(..),    -- instance of: Eq, Show
        dataTypeRep,    -- :: DataType -> DataRep
        -- ** Convenience functions
        repConstr,      -- :: DataType -> ConstrRep -> Constr
        isAlgType,      -- :: DataType -> Bool
        dataTypeConstrs,-- :: DataType -> [Constr]
        indexConstr,    -- :: DataType -> ConIndex -> Constr
        maxConstrIndex, -- :: DataType -> ConIndex
        isNorepType,    -- :: DataType -> Bool

        -- * Data constructor representations
        Constr,         -- abstract, instance of: Eq, Show
        ConIndex,       -- alias for Int, start at 1
        Fixity(..),     -- instance of: Eq, Show
        -- ** Constructors
        mkConstr,       -- :: DataType -> String -> Fixity -> Constr
        mkIntConstr,    -- :: DataType -> Integer -> Constr
        mkFloatConstr,  -- :: DataType -> Double  -> Constr
        mkStringConstr, -- :: DataType -> String  -> Constr
        -- ** Observers
        constrType,     -- :: Constr   -> DataType
        ConstrRep(..),  -- instance of: Eq, Show
        constrRep,      -- :: Constr   -> ConstrRep
        constrFields,   -- :: Constr   -> [String]
        constrFixity,   -- :: Constr   -> Fixity
        -- ** Convenience function: algebraic data types
        constrIndex,    -- :: Constr   -> ConIndex
        -- ** From strings to constructors and vice versa: all data types
        showConstr,     -- :: Constr   -> String
        readConstr,     -- :: DataType -> String -> Maybe Constr

        -- * Convenience functions: take type constructors apart
        tyconUQname,    -- :: String -> String
        tyconModule,    -- :: String -> String

        -- * Generic operations defined in terms of 'gunfold'
        fromConstr,     -- :: Constr -> a
        fromConstrB,    -- :: ... -> Constr -> a
        fromConstrM     -- :: Monad m => ... -> Constr -> m a

  ) where


------------------------------------------------------------------------------

import Prelude -- necessary to get dependencies right

import Data.Typeable
import Data.Maybe
import Control.Monad

-- Imports for the instances
import Data.Typeable
import Data.Int              -- So we can give Data instance for Int8, ...
import Data.Word             -- So we can give Data instance for Word8, ...
#ifdef __GLASGOW_HASKELL__
import GHC.Real( Ratio(..) ) -- So we can give Data instance for Ratio
--import GHC.IOBase            -- So we can give Data instance for IO, Handle
import GHC.Ptr               -- So we can give Data instance for Ptr
import GHC.ForeignPtr        -- So we can give Data instance for ForeignPtr
--import GHC.Stable            -- So we can give Data instance for StablePtr
--import GHC.ST                -- So we can give Data instance for ST
--import GHC.Conc              -- So we can give Data instance for MVar & Co.
import GHC.Arr               -- So we can give Data instance for Array
#else
# ifdef __HUGS__
import Hugs.Prelude( Ratio(..) )
# endif
import System.IO
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.StablePtr
import Control.Monad.ST
import Control.Concurrent
import Data.Array
import Data.IORef
#endif

#include "Typeable.h"



------------------------------------------------------------------------------
--
--      The Data class
--
------------------------------------------------------------------------------

{- |
The 'Data' class comprehends a fundamental primitive 'gfoldl' for
folding over constructor applications, say terms. This primitive can
be instantiated in several ways to map over the immediate subterms
of a term; see the @gmap@ combinators later in this class.  Indeed, a
generic programmer does not necessarily need to use the ingenious gfoldl
primitive but rather the intuitive @gmap@ combinators.  The 'gfoldl'
primitive is completed by means to query top-level constructors, to
turn constructor representations into proper terms, and to list all
possible datatype constructors.  This completion allows us to serve
generic programming scenarios like read, show, equality, term generation.

The combinators 'gmapT', 'gmapQ', 'gmapM', etc are all provided with
default definitions in terms of 'gfoldl', leaving open the opportunity
to provide datatype-specific definitions.
(The inclusion of the @gmap@ combinators as members of class 'Data'
allows the programmer or the compiler to derive specialised, and maybe
more efficient code per datatype.  /Note/: 'gfoldl' is more higher-order
than the @gmap@ combinators.  This is subject to ongoing benchmarking
experiments.  It might turn out that the @gmap@ combinators will be
moved out of the class 'Data'.)

Conceptually, the definition of the @gmap@ combinators in terms of the
primitive 'gfoldl' requires the identification of the 'gfoldl' function
arguments.  Technically, we also need to identify the type constructor
@c@ for the construction of the result type from the folded term type.

In the definition of @gmapQ@/x/ combinators, we use phantom type
constructors for the @c@ in the type of 'gfoldl' because the result type
of a query does not involve the (polymorphic) type of the term argument.
In the definition of 'gmapQl' we simply use the plain constant type
constructor because 'gfoldl' is left-associative anyway and so it is
readily suited to fold a left-associative binary operation over the
immediate subterms.  In the definition of gmapQr, extra effort is
needed. We use a higher-order accumulation trick to mediate between
left-associative constructor application vs. right-associative binary
operation (e.g., @(:)@).  When the query is meant to compute a value
of type @r@, then the result type withing generic folding is @r -> r@.
So the result of folding is a function to which we finally pass the
right unit.

With the @-XDeriveDataTypeable@ option, GHC can generate instances of the
'Data' class automatically.  For example, given the declaration

> data T a b = C1 a b | C2 deriving (Typeable, Data)

GHC will generate an instance that is equivalent to

> instance (Data a, Data b) => Data (T a b) where
>     gfoldl k z (C1 a b) = z C1 `k` a `k` b
>     gfoldl k z C2       = z C2
>
>     gunfold k z c = case constrIndex c of
>                         1 -> k (k (z C1))
>                         2 -> z C2
>
>     toConstr (C1 _ _) = con_C1
>     toConstr C2       = con_C2
>
>     dataTypeOf _ = ty_T
>
> con_C1 = mkConstr ty_T "C1" [] Prefix
> con_C2 = mkConstr ty_T "C2" [] Prefix
> ty_T   = mkDataType "Module.T" [con_C1, con_C2]

This is suitable for datatypes that are exported transparently.

-}

class Typeable a => Data a where

  -- | Left-associative fold operation for constructor applications.
  --
  -- The type of 'gfoldl' is a headache, but operationally it is a simple
  -- generalisation of a list fold.
  --
  -- The default definition for 'gfoldl' is @'const' 'id'@, which is
  -- suitable for abstract datatypes with no substructures.
  gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)
                -- ^ defines how nonempty constructor applications are
                -- folded.  It takes the folded tail of the constructor
                -- application and its head, i.e., an immediate subterm,
                -- and combines them in some way.
          -> (forall g. g -> c g)
                -- ^ defines how the empty constructor application is
                -- folded, like the neutral \/ start element for list
                -- folding.
          -> a
                -- ^ structure to be folded.
          -> c a
                -- ^ result, with a type defined in terms of @a@, but
                -- variability is achieved by means of type constructor
                -- @c@ for the construction of the actual result type.

  -- See the 'Data' instances in this file for an illustration of 'gfoldl'.

  gfoldl _ z = z

  -- | Unfolding constructor applications
  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a

  -- | Obtaining the constructor from a given datum.
  -- For proper terms, this is meant to be the top-level constructor.
  -- Primitive datatypes are here viewed as potentially infinite sets of
  -- values (i.e., constructors).
  toConstr   :: a -> Constr


  -- | The outer type constructor of the type
  dataTypeOf  :: a -> DataType



------------------------------------------------------------------------------
--
-- Mediate types and type constructors
--
------------------------------------------------------------------------------

  -- | Mediate types and unary type constructors.
  -- In 'Data' instances of the form @T a@, 'dataCast1' should be defined
  -- as 'gcast1'.
  --
  -- The default definition is @'const' 'Nothing'@, which is appropriate
  -- for non-unary type constructors.
  dataCast1 :: Typeable1 t
            => (forall d. Data d => c (t d))
            -> Maybe (c a)
  dataCast1 _ = Nothing

  -- | Mediate types and binary type constructors.
  -- In 'Data' instances of the form @T a b@, 'dataCast2' should be
  -- defined as 'gcast2'.
  --
  -- The default definition is @'const' 'Nothing'@, which is appropriate
  -- for non-binary type constructors.
  dataCast2 :: Typeable2 t
            => (forall d e. (Data d, Data e) => c (t d e))
            -> Maybe (c a)
  dataCast2 _ = Nothing



------------------------------------------------------------------------------
--
--      Typical generic maps defined in terms of gfoldl
--
------------------------------------------------------------------------------


  -- | A generic transformation that maps over the immediate subterms
  --
  -- The default definition instantiates the type constructor @c@ in the
  -- type of 'gfoldl' to an identity datatype constructor, using the
  -- isomorphism pair as injection and projection.
  gmapT :: (forall b. Data b => b -> b) -> a -> a

  -- Use an identity datatype constructor ID (see below)
  -- to instantiate the type constructor c in the type of gfoldl,
  -- and perform injections ID and projections unID accordingly.
  --
  gmapT f x0 = unID (gfoldl k ID x0)
    where
      k (ID c) x = ID (c (f x))


  -- | A generic query with a left-associative binary operator
  gmapQl :: (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQl o r f = unCONST . gfoldl k z
    where
      k c x = CONST $ (unCONST c) `o` f x
      z _   = CONST r

  -- | A generic query with a right-associative binary operator
  gmapQr :: (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQr o r0 f x0 = unQr (gfoldl k (const (Qr id)) x0) r0
    where
      k (Qr c) x = Qr (\r -> c (f x `o` r))


  -- | A generic query that processes the immediate subterms and returns a list
  -- of results.  The list is given in the same order as originally specified
  -- in the declaratoin of the data constructors.
  gmapQ :: (forall d. Data d => d -> u) -> a -> [u]
  gmapQ f = gmapQr (:) [] f


  -- | A generic query that processes one child by index (zero-based)
  gmapQi :: Int -> (forall d. Data d => d -> u) -> a -> u
  gmapQi i f x = case gfoldl k z x of { Qi _ q -> fromJust q }
    where
      k (Qi i' q) a = Qi (i'+1) (if i==i' then Just (f a) else q)
      z _           = Qi 0 Nothing


  -- | A generic monadic transformation that maps over the immediate subterms
  --
  -- The default definition instantiates the type constructor @c@ in
  -- the type of 'gfoldl' to the monad datatype constructor, defining
  -- injection and projection using 'return' and '>>='.
  gmapM   :: Monad m => (forall d. Data d => d -> m d) -> a -> m a

  -- Use immediately the monad datatype constructor 
  -- to instantiate the type constructor c in the type of gfoldl,
  -- so injection and projection is done by return and >>=.
  --  
  gmapM f = gfoldl k return
    where
      k c x = do c' <- c
                 x' <- f x
                 return (c' x')


  -- | Transformation of at least one immediate subterm does not fail
  gmapMp :: MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a

{-

The type constructor that we use here simply keeps track of the fact
if we already succeeded for an immediate subterm; see Mp below. To
this end, we couple the monadic computation with a Boolean.

-}

  gmapMp f x = unMp (gfoldl k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z g = Mp (return (g,False))
      k (Mp c) y
        = Mp ( c >>= \(h, b) ->
                 (f y >>= \y' -> return (h y', True))
                 `mplus` return (h y, b)
             )

  -- | Transformation of one immediate subterm with success
  gmapMo :: MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a

{-

We use the same pairing trick as for gmapMp, 
i.e., we use an extra Bool component to keep track of the 
fact whether an immediate subterm was processed successfully.
However, we cut of mapping over subterms once a first subterm
was transformed successfully.

-}

  gmapMo f x = unMp (gfoldl k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z g = Mp (return (g,False))
      k (Mp c) y
        = Mp ( c >>= \(h,b) -> if b
                        then return (h y, b)
                        else (f y >>= \y' -> return (h y',True))
                             `mplus` return (h y, b)
             )


-- | The identity type constructor needed for the definition of gmapT
newtype ID x = ID { unID :: x }


-- | The constant type constructor needed for the definition of gmapQl
newtype CONST c a = CONST { unCONST :: c }


-- | Type constructor for adding counters to queries
data Qi q a = Qi Int (Maybe q)


-- | The type constructor used in definition of gmapQr
newtype Qr r a = Qr { unQr  :: r -> r }


-- | The type constructor used in definition of gmapMp
newtype Mp m x = Mp { unMp :: m (x, Bool) }



------------------------------------------------------------------------------
--
--      Generic unfolding
--
------------------------------------------------------------------------------


-- | Build a term skeleton
fromConstr :: Data a => Constr -> a
fromConstr = fromConstrB undefined


-- | Build a term and use a generic function for subterms
fromConstrB :: Data a
            => (forall d. Data d => d)
            -> Constr
            -> a
fromConstrB f = unID . gunfold k z
 where
  k c = ID (unID c f)
  z = ID


-- | Monadic variation on 'fromConstrB'
fromConstrM :: (Monad m, Data a)
            => (forall d. Data d => m d)
            -> Constr
            -> m a
fromConstrM f = gunfold k z
 where
  k c = do { c' <- c; b <- f; return (c' b) }
  z = return



------------------------------------------------------------------------------
--
--      Datatype and constructor representations
--
------------------------------------------------------------------------------


--
-- | Representation of datatypes.
-- A package of constructor representations with names of type and module.
--
data DataType = DataType
                        { tycon   :: String
                        , datarep :: DataRep
                        }

              deriving Show


-- | Representation of constructors
data Constr = Constr
                        { conrep    :: ConstrRep
                        , constring :: String
                        , confields :: [String] -- for AlgRep only
                        , confixity :: Fixity   -- for AlgRep only
                        , datatype  :: DataType
                        }

instance Show Constr where
 show = constring


-- | Equality of constructors
instance Eq Constr where
  c == c' = constrRep c == constrRep c'


-- | Public representation of datatypes
data DataRep = AlgRep [Constr]
             | IntRep
             | FloatRep
             | StringRep
             | NoRep

            deriving (Eq,Show)
-- The list of constructors could be an array, a balanced tree, or others.


-- | Public representation of constructors
data ConstrRep = AlgConstr    ConIndex
               | IntConstr    Integer
               | FloatConstr  Double
               | StringConstr String

               deriving (Eq,Show)


-- | Unique index for datatype constructors,
-- counting from 1 in the order they are given in the program text.
type ConIndex = Int


-- | Fixity of constructors
data Fixity = Prefix
            | Infix     -- Later: add associativity and precedence

            deriving (Eq,Show)


------------------------------------------------------------------------------
--
--      Observers for datatype representations
--
------------------------------------------------------------------------------


-- | Gets the type constructor including the module
dataTypeName :: DataType -> String
dataTypeName = tycon



-- | Gets the public presentation of a datatype
dataTypeRep :: DataType -> DataRep
dataTypeRep = datarep


-- | Gets the datatype of a constructor
constrType :: Constr -> DataType
constrType = datatype


-- | Gets the public presentation of constructors
constrRep :: Constr -> ConstrRep
constrRep = conrep


-- | Look up a constructor by its representation
repConstr :: DataType -> ConstrRep -> Constr
repConstr dt cr =
      case (dataTypeRep dt, cr) of
        (AlgRep cs, AlgConstr i)      -> cs !! (i-1)
        (IntRep,    IntConstr i)      -> mkIntConstr dt i
        (FloatRep,  FloatConstr f)    -> mkFloatConstr dt f
        (StringRep, StringConstr str) -> mkStringConstr dt str
        _ -> error "repConstr"



------------------------------------------------------------------------------
--
--      Representations of algebraic data types
--
------------------------------------------------------------------------------


-- | Constructs an algebraic datatype
mkDataType :: String -> [Constr] -> DataType
mkDataType str cs = DataType
                        { tycon   = str
                        , datarep = AlgRep cs
                        }


-- | Constructs a constructor
mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dt str fields fix =
        Constr
                { conrep    = AlgConstr idx
                , constring = str
                , confields = fields
                , confixity = fix
                , datatype  = dt
                }
  where
    idx = head [ i | (c,i) <- dataTypeConstrs dt `zip` [1..],
                     showConstr c == str ]


-- | Gets the constructors of an algebraic datatype
dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs dt = case datarep dt of
                        (AlgRep cons) -> cons
                        _ -> error "dataTypeConstrs"


-- | Gets the field labels of a constructor.  The list of labels
-- is returned in the same order as they were given in the original 
-- constructor declaration.
constrFields :: Constr -> [String]
constrFields = confields


-- | Gets the fixity of a constructor
constrFixity :: Constr -> Fixity
constrFixity = confixity



------------------------------------------------------------------------------
--
--      From strings to constr's and vice versa: all data types
--      
------------------------------------------------------------------------------


-- | Gets the string for a constructor
showConstr :: Constr -> String
showConstr = constring


-- | Lookup a constructor via a string
readConstr :: DataType -> String -> Maybe Constr
readConstr dt str =
      case dataTypeRep dt of
        AlgRep cons -> idx cons
        IntRep      -> mkReadCon (\i -> (mkPrimCon dt str (IntConstr i)))
        FloatRep    -> mkReadCon (\f -> (mkPrimCon dt str (FloatConstr f)))
        StringRep   -> Just (mkStringConstr dt str)
        NoRep       -> Nothing
  where

    -- Read a value and build a constructor
    mkReadCon :: Read t => (t -> Constr) -> Maybe Constr
    mkReadCon f = case (reads str) of
                    [(t,"")] -> Just (f t)
                    _ -> Nothing

    -- Traverse list of algebraic datatype constructors
    idx :: [Constr] -> Maybe Constr
    idx cons = let fit = filter ((==) str . showConstr) cons
                in if fit == []
                     then Nothing
                     else Just (head fit)


------------------------------------------------------------------------------
--
--      Convenience funtions: algebraic data types
--
------------------------------------------------------------------------------


-- | Test for an algebraic type
isAlgType :: DataType -> Bool
isAlgType dt = case datarep dt of
                 (AlgRep _) -> True
                 _ -> False


-- | Gets the constructor for an index (algebraic datatypes only)
indexConstr :: DataType -> ConIndex -> Constr
indexConstr dt idx = case datarep dt of
                        (AlgRep cs) -> cs !! (idx-1)
                        _           -> error "indexConstr"


-- | Gets the index of a constructor (algebraic datatypes only)
constrIndex :: Constr -> ConIndex
constrIndex con = case constrRep con of
                    (AlgConstr idx) -> idx
                    _ -> error "constrIndex"


-- | Gets the maximum constructor index of an algebraic datatype
maxConstrIndex :: DataType -> ConIndex
maxConstrIndex dt = case dataTypeRep dt of
                        AlgRep cs -> length cs
                        _            -> error "maxConstrIndex"



------------------------------------------------------------------------------
--
--      Representation of primitive types
--
------------------------------------------------------------------------------


-- | Constructs the 'Int' type
mkIntType :: String -> DataType
mkIntType = mkPrimType IntRep


-- | Constructs the 'Float' type
mkFloatType :: String -> DataType
mkFloatType = mkPrimType FloatRep


-- | Constructs the 'String' type
mkStringType :: String -> DataType
mkStringType = mkPrimType StringRep


-- | Helper for 'mkIntType', 'mkFloatType', 'mkStringType'
mkPrimType :: DataRep -> String -> DataType
mkPrimType dr str = DataType
                        { tycon   = str
                        , datarep = dr
                        }


-- Makes a constructor for primitive types
mkPrimCon :: DataType -> String -> ConstrRep -> Constr
mkPrimCon dt str cr = Constr
                        { datatype  = dt
                        , conrep    = cr
                        , constring = str
                        , confields = error "constrFields"
                        , confixity = error "constrFixity"
                        }


mkIntConstr :: DataType -> Integer -> Constr
mkIntConstr dt i = case datarep dt of
                  IntRep -> mkPrimCon dt (show i) (IntConstr i)
                  _ -> error "mkIntConstr"


mkFloatConstr :: DataType -> Double -> Constr
mkFloatConstr dt f = case datarep dt of
                    FloatRep -> mkPrimCon dt (show f) (FloatConstr f)
                    _ -> error "mkFloatConstr"


mkStringConstr :: DataType -> String -> Constr
mkStringConstr dt str = case datarep dt of
                       StringRep -> mkPrimCon dt str (StringConstr str)
                       _ -> error "mkStringConstr"


------------------------------------------------------------------------------
--
--      Non-representations for non-presentable types
--
------------------------------------------------------------------------------


-- | Constructs a non-representation for a non-presentable type
mkNorepType :: String -> DataType
mkNorepType str = DataType
                        { tycon   = str
                        , datarep = NoRep
                        }


-- | Test for a non-representable type
isNorepType :: DataType -> Bool
isNorepType dt = case datarep dt of
                   NoRep -> True
                   _ -> False



------------------------------------------------------------------------------
--
--      Convenience for qualified type constructors
--
------------------------------------------------------------------------------


-- | Gets the unqualified type constructor:
-- drop *.*.*... before name
--
tyconUQname :: String -> String
tyconUQname x = let x' = dropWhile (not . (==) '.') x
                 in if x' == [] then x else tyconUQname (tail x')


-- | Gets the module of a type constructor:
-- take *.*.*... before name
tyconModule :: String -> String
tyconModule x = let (a,b) = break ((==) '.') x
                 in if b == ""
                      then b
                      else a ++ tyconModule' (tail b)
  where
    tyconModule' y = let y' = tyconModule y
                      in if y' == "" then "" else ('.':y')




------------------------------------------------------------------------------
------------------------------------------------------------------------------
--
--      Instances of the Data class for Prelude-like types.
--      We define top-level definitions for representations.
--
------------------------------------------------------------------------------


falseConstr :: Constr
falseConstr  = mkConstr boolDataType "False" [] Prefix
trueConstr :: Constr
trueConstr   = mkConstr boolDataType "True"  [] Prefix

boolDataType :: DataType
boolDataType = mkDataType "Prelude.Bool" [falseConstr,trueConstr]

instance Data Bool where
  toConstr False = falseConstr
  toConstr True  = trueConstr
  gunfold _ z c  = case constrIndex c of
                     1 -> z False
                     2 -> z True
                     _ -> error "gunfold"
  dataTypeOf _ = boolDataType


------------------------------------------------------------------------------

charType :: DataType
charType = mkStringType "Prelude.Char"

instance Data Char where
  toConstr x = mkStringConstr charType [x]
  gunfold _ z c = case constrRep c of
                    (StringConstr [x]) -> z x
                    _ -> error "gunfold"
  dataTypeOf _ = charType


------------------------------------------------------------------------------

floatType :: DataType
floatType = mkFloatType "Prelude.Float"

instance Data Float where
  toConstr x = mkFloatConstr floatType (realToFrac x)
  gunfold _ z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> error "gunfold"
  dataTypeOf _ = floatType


------------------------------------------------------------------------------

doubleType :: DataType
doubleType = mkFloatType "Prelude.Double"

instance Data Double where
  toConstr = mkFloatConstr floatType
  gunfold _ z c = case constrRep c of
                    (FloatConstr x) -> z x
                    _ -> error "gunfold"
  dataTypeOf _ = doubleType


------------------------------------------------------------------------------

intType :: DataType
intType = mkIntType "Prelude.Int"

instance Data Int where
  toConstr x = mkIntConstr intType (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = intType


------------------------------------------------------------------------------

integerType :: DataType
integerType = mkIntType "Prelude.Integer"

instance Data Integer where
  toConstr = mkIntConstr integerType
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z x
                    _ -> error "gunfold"
  dataTypeOf _ = integerType


------------------------------------------------------------------------------

int8Type :: DataType
int8Type = mkIntType "Data.Int.Int8"

instance Data Int8 where
  toConstr x = mkIntConstr int8Type (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = int8Type


------------------------------------------------------------------------------

int16Type :: DataType
int16Type = mkIntType "Data.Int.Int16"

instance Data Int16 where
  toConstr x = mkIntConstr int16Type (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = int16Type


------------------------------------------------------------------------------

int32Type :: DataType
int32Type = mkIntType "Data.Int.Int32"

instance Data Int32 where
  toConstr x = mkIntConstr int32Type (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = int32Type


------------------------------------------------------------------------------

int64Type :: DataType
int64Type = mkIntType "Data.Int.Int64"

instance Data Int64 where
  toConstr x = mkIntConstr int64Type (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = int64Type


------------------------------------------------------------------------------

wordType :: DataType
wordType = mkIntType "Data.Word.Word"

instance Data Word where
  toConstr x = mkIntConstr wordType (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = wordType


------------------------------------------------------------------------------

word8Type :: DataType
word8Type = mkIntType "Data.Word.Word8"

instance Data Word8 where
  toConstr x = mkIntConstr word8Type (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = word8Type


------------------------------------------------------------------------------

word16Type :: DataType
word16Type = mkIntType "Data.Word.Word16"

instance Data Word16 where
  toConstr x = mkIntConstr word16Type (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = word16Type


------------------------------------------------------------------------------

word32Type :: DataType
word32Type = mkIntType "Data.Word.Word32"

instance Data Word32 where
  toConstr x = mkIntConstr word32Type (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = word32Type


------------------------------------------------------------------------------

word64Type :: DataType
word64Type = mkIntType "Data.Word.Word64"

instance Data Word64 where
  toConstr x = mkIntConstr word64Type (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = word64Type


------------------------------------------------------------------------------

ratioConstr :: Constr
ratioConstr = mkConstr ratioDataType ":%" [] Infix

ratioDataType :: DataType
ratioDataType = mkDataType "GHC.Real.Ratio" [ratioConstr]

instance (Data a, Integral a) => Data (Ratio a) where
  gfoldl k z (a :% b) = z (:%) `k` a `k` b
  toConstr _ = ratioConstr
  gunfold k z c | constrIndex c == 1 = k (k (z (:%)))
  gunfold _ _ _ = error "gunfold"
  dataTypeOf _  = ratioDataType


------------------------------------------------------------------------------

nilConstr :: Constr
nilConstr    = mkConstr listDataType "[]" [] Prefix
consConstr :: Constr
consConstr   = mkConstr listDataType "(:)" [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

instance Data a => Data [a] where
  gfoldl _ z []     = z []
  gfoldl f z (x:xs) = z (:) `f` x `f` xs
  toConstr []    = nilConstr
  toConstr (_:_) = consConstr
  gunfold k z c = case constrIndex c of
                    1 -> z []
                    2 -> k (k (z (:)))
                    _ -> error "gunfold"
  dataTypeOf _ = listDataType
  dataCast1 f  = gcast1 f

--
-- The gmaps are given as an illustration.
-- This shows that the gmaps for lists are different from list maps.
--
  gmapT  _   []     = []
  gmapT  f   (x:xs) = (f x:f xs)
  gmapQ  _   []     = []
  gmapQ  f   (x:xs) = [f x,f xs]
  gmapM  _   []     = return []
  gmapM  f   (x:xs) = f x >>= \x' -> f xs >>= \xs' -> return (x':xs')


------------------------------------------------------------------------------

nothingConstr :: Constr
nothingConstr = mkConstr maybeDataType "Nothing" [] Prefix
justConstr :: Constr
justConstr    = mkConstr maybeDataType "Just"    [] Prefix

maybeDataType :: DataType
maybeDataType = mkDataType "Prelude.Maybe" [nothingConstr,justConstr]

instance Data a => Data (Maybe a) where
  gfoldl _ z Nothing  = z Nothing
  gfoldl f z (Just x) = z Just `f` x
  toConstr Nothing  = nothingConstr
  toConstr (Just _) = justConstr
  gunfold k z c = case constrIndex c of
                    1 -> z Nothing
                    2 -> k (z Just)
                    _ -> error "gunfold"
  dataTypeOf _ = maybeDataType
  dataCast1 f  = gcast1 f


------------------------------------------------------------------------------

ltConstr :: Constr
ltConstr         = mkConstr orderingDataType "LT" [] Prefix
eqConstr :: Constr
eqConstr         = mkConstr orderingDataType "EQ" [] Prefix
gtConstr :: Constr
gtConstr         = mkConstr orderingDataType "GT" [] Prefix

orderingDataType :: DataType
orderingDataType = mkDataType "Prelude.Ordering" [ltConstr,eqConstr,gtConstr]

instance Data Ordering where
  gfoldl _ z LT  = z LT
  gfoldl _ z EQ  = z EQ
  gfoldl _ z GT  = z GT
  toConstr LT  = ltConstr
  toConstr EQ  = eqConstr
  toConstr GT  = gtConstr
  gunfold _ z c = case constrIndex c of
                    1 -> z LT
                    2 -> z EQ
                    3 -> z GT
                    _ -> error "gunfold"
  dataTypeOf _ = orderingDataType


------------------------------------------------------------------------------

leftConstr :: Constr
leftConstr     = mkConstr eitherDataType "Left"  [] Prefix

rightConstr :: Constr
rightConstr    = mkConstr eitherDataType "Right" [] Prefix

eitherDataType :: DataType
eitherDataType = mkDataType "Prelude.Either" [leftConstr,rightConstr]

instance (Data a, Data b) => Data (Either a b) where
  gfoldl f z (Left a)   = z Left  `f` a
  gfoldl f z (Right a)  = z Right `f` a
  toConstr (Left _)  = leftConstr
  toConstr (Right _) = rightConstr
  gunfold k z c = case constrIndex c of
                    1 -> k (z Left)
                    2 -> k (z Right)
                    _ -> error "gunfold"
  dataTypeOf _ = eitherDataType
  dataCast2 f  = gcast2 f


------------------------------------------------------------------------------


--
-- A last resort for functions
--

instance (Data a, Data b) => Data (a -> b) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "Prelude.(->)"
  dataCast2 f  = gcast2 f


------------------------------------------------------------------------------

tuple0Constr :: Constr
tuple0Constr = mkConstr tuple0DataType "()" [] Prefix

tuple0DataType :: DataType
tuple0DataType = mkDataType "Prelude.()" [tuple0Constr]

instance Data () where
  toConstr ()   = tuple0Constr
  gunfold _ z c | constrIndex c == 1 = z ()
  gunfold _ _ _ = error "gunfold"
  dataTypeOf _  = tuple0DataType


------------------------------------------------------------------------------

tuple2Constr :: Constr
tuple2Constr = mkConstr tuple2DataType "(,)" [] Infix

tuple2DataType :: DataType
tuple2DataType = mkDataType "Prelude.(,)" [tuple2Constr]

instance (Data a, Data b) => Data (a,b) where
  gfoldl f z (a,b) = z (,) `f` a `f` b
  toConstr (_,_) = tuple2Constr
  gunfold k z c | constrIndex c == 1 = k (k (z (,)))
  gunfold _ _ _ = error "gunfold"
  dataTypeOf _  = tuple2DataType
  dataCast2 f   = gcast2 f


------------------------------------------------------------------------------

tuple3Constr :: Constr
tuple3Constr = mkConstr tuple3DataType "(,,)" [] Infix

tuple3DataType :: DataType
tuple3DataType = mkDataType "Prelude.(,)" [tuple3Constr]

instance (Data a, Data b, Data c) => Data (a,b,c) where
  gfoldl f z (a,b,c) = z (,,) `f` a `f` b `f` c
  toConstr (_,_,_) = tuple3Constr
  gunfold k z c | constrIndex c == 1 = k (k (k (z (,,))))
  gunfold _ _ _ = error "gunfold"
  dataTypeOf _  = tuple3DataType


------------------------------------------------------------------------------

tuple4Constr :: Constr
tuple4Constr = mkConstr tuple4DataType "(,,,)" [] Infix

tuple4DataType :: DataType
tuple4DataType = mkDataType "Prelude.(,,,)" [tuple4Constr]

instance (Data a, Data b, Data c, Data d)
         => Data (a,b,c,d) where
  gfoldl f z (a,b,c,d) = z (,,,) `f` a `f` b `f` c `f` d
  toConstr (_,_,_,_) = tuple4Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (z (,,,)))))
                    _ -> error "gunfold"
  dataTypeOf _ = tuple4DataType


------------------------------------------------------------------------------

tuple5Constr :: Constr
tuple5Constr = mkConstr tuple5DataType "(,,,,)" [] Infix

tuple5DataType :: DataType
tuple5DataType = mkDataType "Prelude.(,,,,)" [tuple5Constr]

instance (Data a, Data b, Data c, Data d, Data e)
         => Data (a,b,c,d,e) where
  gfoldl f z (a,b,c,d,e) = z (,,,,) `f` a `f` b `f` c `f` d `f` e
  toConstr (_,_,_,_,_) = tuple5Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (k (z (,,,,))))))
                    _ -> error "gunfold"
  dataTypeOf _ = tuple5DataType


------------------------------------------------------------------------------

tuple6Constr :: Constr
tuple6Constr = mkConstr tuple6DataType "(,,,,,)" [] Infix

tuple6DataType :: DataType
tuple6DataType = mkDataType "Prelude.(,,,,,)" [tuple6Constr]

instance (Data a, Data b, Data c, Data d, Data e, Data f)
         => Data (a,b,c,d,e,f) where
  gfoldl f z (a,b,c,d,e,f') = z (,,,,,) `f` a `f` b `f` c `f` d `f` e `f` f'
  toConstr (_,_,_,_,_,_) = tuple6Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (k (k (z (,,,,,)))))))
                    _ -> error "gunfold"
  dataTypeOf _ = tuple6DataType


------------------------------------------------------------------------------

tuple7Constr :: Constr
tuple7Constr = mkConstr tuple7DataType "(,,,,,,)" [] Infix

tuple7DataType :: DataType
tuple7DataType = mkDataType "Prelude.(,,,,,,)" [tuple7Constr]

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g)
         => Data (a,b,c,d,e,f,g) where
  gfoldl f z (a,b,c,d,e,f',g) =
    z (,,,,,,) `f` a `f` b `f` c `f` d `f` e `f` f' `f` g
  toConstr  (_,_,_,_,_,_,_) = tuple7Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (k (k (k (z (,,,,,,))))))))
                    _ -> error "gunfold"
  dataTypeOf _ = tuple7DataType


------------------------------------------------------------------------------

instance Typeable a => Data (Ptr a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.Ptr.Ptr"


------------------------------------------------------------------------------

instance Typeable a => Data (ForeignPtr a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.ForeignPtr.ForeignPtr"


------------------------------------------------------------------------------
-- The Data instance for Array preserves data abstraction at the cost of 
-- inefficiency. We omit reflection services for the sake of data abstraction.
instance (Typeable a, Data b, Ix a) => Data (Array a b)
 where
  gfoldl f z a = z (listArray (bounds a)) `f` (elems a)
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "Data.Array.Array"
