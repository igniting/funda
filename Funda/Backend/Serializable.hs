{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Funda.Backend.Serializable where

class Serializable from to where
  encode :: from -> to
  decode :: to -> Either String from

instance Serializable t t where
  encode = id
  decode = Right
