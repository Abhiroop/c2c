module CAP where

import Data.Word as W

{-
   The capability machine from:
   "Reasoning About a Machine with Local Capabilities - Provably Safe Stsck and Return Pointer Management"
   - Skorstengaard, Devriese, Birkedal. ESOP 2018

-}

-- | A plain machine address
type Addr = W.Word

-- | Permissions
data Perm = O   | RO   | RW | RX | RWX
          | RWL | RWLX | E
          deriving (Show, Eq)

-- | Tag indicating global or local capability
data Global = Global | Local deriving (Show, Eq)

type Base = Addr

-- does not support infinite address space, unlike the paper
type End  = Addr

-- | A capability (or a pointer in CAP machines)
-- Consists of permissions on the capability and a tag indicating
-- if its a local or global capability.
-- Also, metadata on bounds are included.
data Cap = Cap (Perm, Global) Base End Addr deriving (Show, Eq)

-- | A machine word in a CAP machine.
-- Either a plain address or a capability
data Word = A Addr | C Cap deriving (Show, Eq)
