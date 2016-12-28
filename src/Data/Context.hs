{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Context where

import Control.Lens.Lens (Lens, lens)
import Data.Proxy

type family TypeEq a b where
    TypeEq a a = 'True
    TypeEq a b = 'False

type family Subtract (l :: [*]) (x :: *) where
    Subtract (x ': t) x = t
    Subtract (y ': t) x = y ': Subtract t x
    Subtract '[] x = '[]

type family Add (l :: [*]) (x :: *) where
    Add xs x = x ': xs

type family Contains (l :: [*]) (x :: *) where
    Contains (x ': xs) x = 'True
    Contains (y ': xs) x = Contains xs x
    Contains z x = 'False

data Context t where
    EmptyContext :: Context '[]
    (:.) :: Contains t a ~ 'False => a -> Context t -> Context (a ': t)

infixr 5 :.

instance Show (Context '[]) where
    show EmptyContext = "EmptyContext"

instance (Show a, Show (Context as)) => Show (Context (a ': as)) where
    showsPrec outerPrecedence (a :. as) =
        showParen (outerPrecedence > 5) $ shows a . showString " :. " . shows as

class HasContextEntry (context :: [*]) (val :: *) where
    getContextEntry :: Context context -> val
    removeContextEntry :: Context context -> Proxy val -> Context (Subtract context val)
    contextEntry :: Contains (Subtract context val) y ~ 'False => Proxy val -> Lens (Context context) (Context (Add (Subtract context val) y)) val y

instance {-# OVERLAPPABLE #-} (
        HasContextEntry xs val,
        (Subtract (notIt ': xs) val) ~ (notIt ': Subtract xs val),
        Contains (Subtract xs val) notIt ~ 'False) => HasContextEntry (notIt ': xs) val where
    getContextEntry (_ :. xs) = getContextEntry xs
    removeContextEntry (y :. xs) x = y :. removeContextEntry xs x
    contextEntry p = lens getContextEntry set'
        where set' (y :. xs) x = x :. y :. removeContextEntry xs p

instance {-# OVERLAPPING #-} ((Subtract xs val) ~ xs, Contains xs val ~ 'False) => HasContextEntry (val ': xs) val where
    getContextEntry (x :. _) = x
    removeContextEntry (_ :. xs) _ = xs
    contextEntry p = lens getContextEntry set'
        where
        set' :: Contains xs y ~ 'False => Context (x ': xs) -> y -> Context (y ': xs)
        set' (_ :. xs) x = x :. xs
