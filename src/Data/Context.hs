{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Context where

import Control.Lens.Lens (Lens', lens)
import Data.Proxy

type family TypeEq a b where
    TypeEq a a = 'True
    TypeEq a b = 'False

type family Mutate (l :: [*]) (a :: *) (b :: *) where
    Mutate (a ': xs) a b = (b ': xs)
    Mutate (v ': xs) a b = (v ': Mutate xs a b)
    Mutate ('[]) a b = '[]

data Context t where
    EmptyContext :: Context '[]
    (:.) :: HasNotContextEntry t a => a -> Context t -> Context (a ': t)

infixr 5 :.

instance Show (Context '[]) where
    show EmptyContext = "EmptyContext"

instance (Show a, Show (Context as)) => Show (Context (a ': as)) where
    showsPrec outerPrecedence (a :. as) =
        showParen (outerPrecedence > 5) $ shows a . showString " :. " . shows as

class HasContextEntry (context :: [*]) (val :: *) where
    getContextEntry :: Context context -> val
    setContextEntry :: Context context -> val -> Context context
    contextEntry :: Lens' (Context context) val

instance {-# OVERLAPPABLE #-} HasContextEntry xs val => HasContextEntry (notIt ': xs) val where
    getContextEntry (_ :. xs) = getContextEntry xs
    setContextEntry (x :. xs) x' = x :. setContextEntry xs x'
    contextEntry = lens getContextEntry setContextEntry

instance {-# OVERLAPPING #-} HasContextEntry (val ': xs) val where
    getContextEntry (x :. _) = x
    setContextEntry (_ :. xs) x = x :. xs
    contextEntry = lens getContextEntry setContextEntry

class HasNotContextEntry (context :: [*]) (val :: *)

instance {-# OVERLAPPABLE #-} (TypeEq n v ~ 'False, HasNotContextEntry xs v) => HasNotContextEntry (n ': xs) v
instance {-# OVERLAPPING #-} HasNotContextEntry '[] v

test :: HasNotContextEntry y z => Context (x ': y) -> z -> Context (z ': y)
test (_ :. y) z = z :. y
