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
{-# LANGUAGE UndecidableInstances #-}

module Data.Context where

import Control.Lens ((&), (.~), (^.))
import Control.Lens.Lens (Lens, lens)
import Data.Proxy

type family TypeEq a b where
    TypeEq a a = 'True
    TypeEq a b = 'False

type family Subtract (l :: [*]) (x :: *) where
    Subtract (x ': t) x = t
    Subtract (y ': t) x = y ': Subtract t x
    Subtract '[] x = '[]

type family Mutate (l :: [*]) (a :: *) (b :: *) where
    Mutate xs a a = xs
    Mutate (a ': xs) a b = b ': Mutate xs a b
    Mutate (c ': xs) a b = c ': Mutate xs a b
    Mutate '[] a b = '[]

type family (a :: Bool) :||: (b :: Bool)  where
    'True :||: b = 'True
    a :||: 'True = 'True
    a :||: b = 'False

type family Contains (l :: [*]) (x :: *) where
    Contains (x ': xs) x = 'True
    Contains (y ': xs) x = Contains xs x
    Contains z x = 'False

type family Intersects (l :: [*]) (r :: [*]) where
    Intersects l (r ': rs) = (Contains l r) :||: (Intersects l rs)
    Intersects l r = 'False

data Context t where
    EmptyContext :: Context '[]
    (:.) :: Contains t a ~ 'False => a -> Context t -> Context (a ': t)

infixr 5 :.

instance Show (Context '[]) where
    show EmptyContext = "EmptyContext"

instance (Show a, Show (Context as)) => Show (Context (a ': as)) where
    showsPrec outerPrecedence (a :. as) =
        showParen (outerPrecedence > 5) $ shows a . showString " :. " . shows as

class RemoveContextEntry (c :: [*]) (a :: *) where
    removeContextEntry :: Context c -> Proxy a -> Context (Subtract c a)

instance {-# OVERLAPPABLE #-} (RemoveContextEntry xs a, (Subtract (x ': xs) a) ~ (x ': Subtract xs a), Contains (Subtract xs a) x ~ 'False) => RemoveContextEntry (x ': xs) a where
    removeContextEntry (x :. xs) p = x :. removeContextEntry xs p

instance {-# OVERLAPPING #-} RemoveContextEntry (a ': xs) a where
    removeContextEntry (_ :. xs) _ = xs

class HasContextLens (c :: [*]) (a :: *) (b :: *) where
    contextLens :: Proxy a -> Lens (Context c) (Context (Mutate c a b)) a b

instance {-# OVERLAPPABLE #-} (TypeEq x a ~ 'False, (Mutate (x ': xs) a b) ~ (x ': Mutate xs a b), Contains (Mutate xs a b) x ~ 'False, HasContextLens xs a b, HasContextLens xs a a) => HasContextLens (x ': xs) a b where
    contextLens p = lens get' (set' p)
        where
        get' :: Context (x ': xs) -> a
        get' (_ :. xs) = xs ^. contextLens p
        set' :: Proxy a -> Context (x ': xs) -> b -> Context (Mutate (x ': xs) a b)
        set' p (x :. xs) b = x :. (xs & contextLens p .~ b)

instance {-# OVERLAPPING #-} (Contains xs b ~ 'False, Mutate (a ': xs) a b ~ (b ': xs)) => HasContextLens (a ': xs) a b where
    contextLens _ = lens get' set'
        where
        get' :: Context (a ': xs) -> a
        get' (x :. _) = x
        set' :: Context (a ': xs) -> b -> Context (b ': xs)
        set' (_ :. xs) b = b :. xs
