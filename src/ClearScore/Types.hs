{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ClearScore.Types 
  ( module T
  , sortingScore
) where

import ClearScore.Types.Internal as T
import ClearScore.Types.Request as T

sortingScore :: Eligibility -> Apr -> Score
sortingScore eligibility' apr' =
  let e = unEligibility eligibility'
      a = unApr apr'
  in Score $ e * ((1 / a) ** 2.0)
