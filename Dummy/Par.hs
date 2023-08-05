{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Dummy.Par
  ( happyError
  , myLexer
  , pProg
  , pListClassDec
  , pListInstDec
  , pListExpr
  , pClassOpDec
  , pClassOpImp
  , pClassDec
  , pInstDec
  , pListClassOpDec
  , pListClassOpImp
  , pList
  , pExpr
  , pTyC
  , pOvType
  , pListTyC
  , pSType
  , pDType
  ) where

import Prelude

import qualified Dummy.Abs
import Dummy.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap20 = HappyWrap20 (String)
happyIn20 :: (String) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (Dummy.Abs.True)
happyIn21 :: (Dummy.Abs.True) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (Dummy.Abs.False)
happyIn22 :: (Dummy.Abs.False) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (Dummy.Abs.Prog)
happyIn23 :: (Dummy.Abs.Prog) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 ([Dummy.Abs.ClassDec])
happyIn24 :: ([Dummy.Abs.ClassDec]) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 ([Dummy.Abs.InstDec])
happyIn25 :: ([Dummy.Abs.InstDec]) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 ([Dummy.Abs.Expr])
happyIn26 :: ([Dummy.Abs.Expr]) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (Dummy.Abs.ClassOpDec)
happyIn27 :: (Dummy.Abs.ClassOpDec) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (Dummy.Abs.ClassOpImp)
happyIn28 :: (Dummy.Abs.ClassOpImp) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 (Dummy.Abs.ClassDec)
happyIn29 :: (Dummy.Abs.ClassDec) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (Dummy.Abs.InstDec)
happyIn30 :: (Dummy.Abs.InstDec) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 ([Dummy.Abs.ClassOpDec])
happyIn31 :: ([Dummy.Abs.ClassOpDec]) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 ([Dummy.Abs.ClassOpImp])
happyIn32 :: ([Dummy.Abs.ClassOpImp]) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 (Dummy.Abs.List)
happyIn33 :: (Dummy.Abs.List) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 (Dummy.Abs.Expr)
happyIn34 :: (Dummy.Abs.Expr) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (Dummy.Abs.TyC)
happyIn35 :: (Dummy.Abs.TyC) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (Dummy.Abs.OvType)
happyIn36 :: (Dummy.Abs.OvType) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 ([Dummy.Abs.TyC])
happyIn37 :: ([Dummy.Abs.TyC]) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (Dummy.Abs.SType)
happyIn38 :: (Dummy.Abs.SType) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (Dummy.Abs.DType)
happyIn39 :: (Dummy.Abs.DType) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x0b\x07\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x08\x08\x00\x00\x00\x00\x00\x00\x0b\x07\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x0c\x10\x00\x00\x00\x00\x00\x88\x01\x02\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x40\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x01\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\xe1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\xb0\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\xc0\xc2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x16\x0e\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\xe1\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x60\x80\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x07\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x2c\x1d\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x70\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x06\x08\x00\x00\x00\x00\x80\x00\x04\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x07\x00\x00\x00\x00\x00\x20\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x20\x00\x20\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x20\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x80\x85\x03\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x08\x00\x00\x00\x00\x00\x02\x0b\x07\x00\x00\x00\x00\x00\x20\x20\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x16\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProg","%start_pListClassDec","%start_pListInstDec","%start_pListExpr","%start_pClassOpDec","%start_pClassOpImp","%start_pClassDec","%start_pInstDec","%start_pListClassOpDec","%start_pListClassOpImp","%start_pList","%start_pExpr","%start_pTyC","%start_pOvType","%start_pListTyC","%start_pSType","%start_pDType","String","True","False","Prog","ListClassDec","ListInstDec","ListExpr","ClassOpDec","ClassOpImp","ClassDec","InstDec","ListClassOpDec","ListClassOpImp","List","Expr","TyC","OvType","ListTyC","SType","DType","','","'->'","':'","';'","'<'","'='","'=>'","'>'","'Bool'","'['","'[]'","'\\\\'","']'","'case'","'class'","'instance'","'of'","'where'","L_quoted","L_True","L_False","%eof"]
        bit_start = st Prelude.* 61
        bit_end = (st Prelude.+ 1) Prelude.* 61
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..60]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xf6\xff\xf6\xff\xfe\xff\xc9\x00\xfc\xff\xfc\xff\x2a\x00\x2c\x00\x28\x00\x28\x00\x6a\x00\xc9\x00\x28\x00\x5d\x00\x53\x00\xd6\x00\x9f\x00\x53\x00\x00\x00\x53\x00\x00\x00\x61\x00\x5e\x00\x70\x00\x00\x00\xd6\x00\xff\xff\xd6\x00\x83\x00\x7c\x00\x7c\x00\x7c\x00\x1f\x00\x00\x00\x00\x00\x00\x00\xb7\x00\x00\x00\xa3\x00\xc9\x00\x00\x00\x00\x00\x90\x00\xa4\x00\xae\x00\xd1\x00\xd0\x00\x93\x00\xdc\x00\xdf\x00\xdf\x00\x40\x00\xdf\x00\xe4\x00\xe0\x00\xe0\x00\xe0\x00\x1c\x00\xe0\x00\xc2\x00\xe2\x00\xee\x00\xe5\x00\xea\x00\xc9\x00\xed\x00\xef\x00\xc9\x00\xc9\x00\xeb\x00\xd6\x00\xeb\x00\x00\x00\xd6\x00\x00\x00\xc9\x00\xeb\x00\xc5\x00\xf2\x00\xc9\x00\xf0\x00\xf7\x00\xd6\x00\xbc\x00\xf5\x00\x00\x00\xf9\x00\x00\x00\xff\x00\x00\x00\xc9\x00\xc9\x00\x6a\x00\x00\x00\xc9\x00\xff\x00\xfa\x00\x24\x00\xf3\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf1\x00\xf1\x00\x00\x01\x04\x01\xc9\x00\xd6\x00\x06\x01\xc9\x00\xf6\x00\x00\x00\x00\x00\xd6\x00\x20\x00\x6a\x00\x2b\x00\xf6\x00\x08\x01\xc9\x00\x00\x00\xc9\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xde\x00\xe1\x00\xe3\x00\x3e\x00\xc8\x00\xdb\x00\x02\x01\x03\x01\x43\x00\xa5\x00\x03\x00\x57\x00\x32\x00\xfc\x00\x02\x00\x06\x00\x04\x00\x00\x00\x00\x00\x0e\x01\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5a\x00\x00\x00\x0f\x01\x5f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x00\x00\x00\x00\x00\xb9\x00\x00\x00\x00\x00\x10\x01\x00\x00\x11\x01\x00\x00\x00\x00\x00\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe6\x00\x47\x00\xe8\x00\xe9\x00\x69\x00\x50\x00\x12\x01\x09\x00\x37\x00\x00\x00\x0a\x00\x00\x00\x6e\x00\x13\x01\x71\x00\x00\x00\x78\x00\x29\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x87\x00\x42\x00\x00\x00\x8a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\x00\xb3\x00\x00\x00\x00\x00\x8a\x00\x0c\x00\x00\x00\x8f\x00\x14\x01\x00\x00\x00\x00\x0d\x00\x99\x00\xaa\x00\x00\x00\xb5\x00\x00\x00\x9e\x00\x00\x00\xa1\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xea\xff\xea\xff\xe7\xff\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdc\xff\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\x00\x00\x00\x00\x00\x00\xee\xff\xc9\xff\xc4\xff\xc3\xff\x00\x00\xcc\xff\xc7\xff\x00\x00\x00\x00\x00\x00\xcb\xff\x00\x00\x00\x00\x00\x00\xd4\xff\xd0\xff\xcf\xff\xd2\xff\x00\x00\xd8\xff\x00\x00\x00\x00\xed\xff\xec\xff\x00\x00\x00\x00\x00\x00\xda\xff\x00\x00\x00\x00\xdc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\x00\x00\xe6\xff\x00\x00\xe9\xff\x00\x00\xe7\xff\xe4\xff\xea\xff\xe7\xff\xd3\xff\xe4\xff\x00\x00\x00\x00\x00\x00\xdb\xff\x00\x00\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\xce\xff\x00\x00\x00\x00\x00\x00\xc8\xff\x00\x00\xc5\xff\xc6\xff\xca\xff\xd6\xff\x00\x00\x00\x00\xd7\xff\xe0\xff\xe1\xff\x00\x00\x00\x00\x00\x00\xe2\xff\xe5\xff\xe8\xff\xeb\xff\xdc\xff\xda\xff\x00\x00\x00\x00\xd5\xff\x00\x00\xcd\xff\x00\x00\x00\x00\xde\xff\xdf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\x00\x00\x00\x00\xdd\xff\xd1\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x13\x00\x0d\x00\x0f\x00\x00\x00\x11\x00\x10\x00\x16\x00\x12\x00\x13\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x04\x00\x0f\x00\x03\x00\x11\x00\x04\x00\x06\x00\x02\x00\x0b\x00\x0c\x00\x00\x00\x0e\x00\x0b\x00\x0c\x00\x02\x00\x0e\x00\x13\x00\x14\x00\x15\x00\x00\x00\x13\x00\x14\x00\x15\x00\x12\x00\x00\x00\x0f\x00\x0f\x00\x11\x00\x13\x00\x10\x00\x12\x00\x00\x00\x01\x00\x02\x00\x0f\x00\x00\x00\x00\x00\x06\x00\x05\x00\x0f\x00\x00\x00\x01\x00\x02\x00\x07\x00\x0d\x00\x0e\x00\x06\x00\x0b\x00\x0d\x00\x00\x00\x01\x00\x02\x00\x13\x00\x0d\x00\x0e\x00\x06\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x05\x00\x02\x00\x0d\x00\x0e\x00\x13\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x16\x00\x0b\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x0e\x00\x13\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x13\x00\x01\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x16\x00\x03\x00\x0d\x00\x0e\x00\x03\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x05\x00\x00\x00\x0d\x00\x0e\x00\x09\x00\x0a\x00\x00\x00\x0d\x00\x0e\x00\x08\x00\x0d\x00\x0e\x00\x00\x00\x0c\x00\x13\x00\x00\x00\x06\x00\x00\x00\x13\x00\x0d\x00\x08\x00\x00\x00\x16\x00\x08\x00\x0c\x00\x08\x00\x02\x00\x0c\x00\x07\x00\x0c\x00\x0b\x00\x0c\x00\x0b\x00\x0e\x00\x04\x00\x00\x00\x00\x00\x0d\x00\x13\x00\x14\x00\x15\x00\x16\x00\x07\x00\x07\x00\x0b\x00\x0c\x00\x0b\x00\x0e\x00\x0b\x00\x0c\x00\x11\x00\x0e\x00\x13\x00\x14\x00\x15\x00\x00\x00\x13\x00\x14\x00\x15\x00\x09\x00\x0a\x00\x03\x00\x04\x00\x08\x00\x13\x00\x04\x00\x16\x00\x09\x00\x05\x00\x13\x00\x09\x00\x05\x00\x04\x00\x0a\x00\x05\x00\x13\x00\x0a\x00\x09\x00\x04\x00\x0a\x00\x02\x00\x16\x00\x16\x00\x13\x00\x16\x00\x02\x00\x10\x00\x16\x00\x0f\x00\x08\x00\x13\x00\x10\x00\x07\x00\x02\x00\x08\x00\x13\x00\x13\x00\x12\x00\x02\x00\x07\x00\x02\x00\x13\x00\x02\x00\x09\x00\x10\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x53\x00\x1b\x00\x2a\x00\x13\x00\x36\x00\x13\x00\x13\x00\x13\x00\x13\x00\x13\x00\x13\x00\x13\x00\x13\x00\x34\x00\x13\x00\x2b\x00\x1c\x00\x1b\x00\x1d\x00\x14\x00\xff\xff\x15\x00\x16\x00\x1a\x00\x53\x00\x51\x00\x61\x00\x5f\x00\x58\x00\x6d\x00\x75\x00\x45\x00\x1c\x00\x4d\x00\x54\x00\x75\x00\x50\x00\x53\x00\x26\x00\x27\x00\x1b\x00\x28\x00\x26\x00\x27\x00\x53\x00\x28\x00\x13\x00\x29\x00\x2a\x00\x1b\x00\x13\x00\x29\x00\x2a\x00\x69\x00\x1b\x00\x1c\x00\x36\x00\x59\x00\x13\x00\x34\x00\x77\x00\x20\x00\x21\x00\x22\x00\x1f\x00\x2a\x00\x2f\x00\x38\x00\x48\x00\x60\x00\x20\x00\x21\x00\x22\x00\x30\x00\x23\x00\x39\x00\x66\x00\x31\x00\x6a\x00\x20\x00\x21\x00\x22\x00\x13\x00\x23\x00\x39\x00\x63\x00\x20\x00\x21\x00\x22\x00\x20\x00\x21\x00\x22\x00\x23\x00\x39\x00\x20\x00\x21\x00\x22\x00\x18\x00\x53\x00\x23\x00\x24\x00\x13\x00\x23\x00\x43\x00\x20\x00\x21\x00\x22\x00\x23\x00\x4d\x00\x20\x00\x21\x00\x22\x00\x20\x00\x21\x00\x22\x00\xff\xff\x26\x00\x23\x00\x43\x00\x20\x00\x21\x00\x22\x00\x23\x00\x5e\x00\x13\x00\x23\x00\x43\x00\x20\x00\x21\x00\x22\x00\x13\x00\x51\x00\x23\x00\x5a\x00\x20\x00\x21\x00\x22\x00\x20\x00\x21\x00\x22\x00\x23\x00\x43\x00\x20\x00\x21\x00\x22\x00\xff\xff\x4d\x00\x23\x00\x6b\x00\x4a\x00\x23\x00\x43\x00\x20\x00\x21\x00\x22\x00\x23\x00\x73\x00\x20\x00\x21\x00\x22\x00\x20\x00\x21\x00\x22\x00\x18\x00\x2c\x00\x23\x00\x43\x00\x19\x00\x1a\x00\x2a\x00\x23\x00\x7a\x00\x2d\x00\x23\x00\x43\x00\x2c\x00\x2e\x00\x13\x00\x2c\x00\x4c\x00\x2c\x00\x13\x00\x77\x00\x2d\x00\x2f\x00\xff\xff\x2d\x00\x4a\x00\x2d\x00\x53\x00\x70\x00\x30\x00\x79\x00\x26\x00\x27\x00\x48\x00\x28\x00\x43\x00\x2f\x00\x2f\x00\x58\x00\x13\x00\x29\x00\x2a\x00\xff\xff\x30\x00\x37\x00\x26\x00\x27\x00\x71\x00\x28\x00\x26\x00\x27\x00\x5d\x00\x28\x00\x13\x00\x29\x00\x2a\x00\x2c\x00\x13\x00\x29\x00\x2a\x00\x19\x00\x1a\x00\x3e\x00\x3f\x00\x36\x00\x13\x00\x3c\x00\xff\xff\x3d\x00\x3a\x00\x13\x00\x3d\x00\x40\x00\x65\x00\x3b\x00\x64\x00\x13\x00\x3b\x00\x3d\x00\x42\x00\x3b\x00\x5c\x00\xff\xff\xff\xff\x13\x00\xff\xff\x53\x00\x34\x00\xff\xff\x36\x00\x57\x00\x13\x00\x34\x00\x6d\x00\x53\x00\x6a\x00\x13\x00\x13\x00\x68\x00\x6f\x00\x70\x00\x53\x00\x13\x00\x79\x00\x34\x00\x1e\x00\x32\x00\x55\x00\x4e\x00\x46\x00\x45\x00\x62\x00\x5d\x00\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (17, 60) [
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60)
	]

happy_n_terms = 23 :: Prelude.Int
happy_n_nonterms = 20 :: Prelude.Int

happyReduce_17 = happySpecReduce_1  0# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn20
		 (happy_var_1
	)}

happyReduce_18 = happySpecReduce_1  1# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_True happy_var_1)) -> 
	happyIn21
		 (Dummy.Abs.True happy_var_1
	)}

happyReduce_19 = happySpecReduce_1  2# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_False happy_var_1)) -> 
	happyIn22
		 (Dummy.Abs.False happy_var_1
	)}

happyReduce_20 = happySpecReduce_3  3# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut25 happy_x_2 of { (HappyWrap25 happy_var_2) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn23
		 (Dummy.Abs.Dummy_Prog happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_21 = happySpecReduce_0  4# happyReduction_21
happyReduction_21  =  happyIn24
		 ([]
	)

happyReduce_22 = happySpecReduce_1  4# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn24
		 ((:[]) happy_var_1
	)}

happyReduce_23 = happySpecReduce_3  4# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	happyIn24
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_24 = happySpecReduce_0  5# happyReduction_24
happyReduction_24  =  happyIn25
		 ([]
	)

happyReduce_25 = happySpecReduce_1  5# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn25
		 ((:[]) happy_var_1
	)}

happyReduce_26 = happySpecReduce_3  5# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	case happyOut25 happy_x_3 of { (HappyWrap25 happy_var_3) -> 
	happyIn25
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_27 = happySpecReduce_0  6# happyReduction_27
happyReduction_27  =  happyIn26
		 ([]
	)

happyReduce_28 = happySpecReduce_1  6# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn26
		 ((:[]) happy_var_1
	)}

happyReduce_29 = happySpecReduce_3  6# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn26
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_30 = happySpecReduce_3  7# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut38 happy_x_3 of { (HappyWrap38 happy_var_3) -> 
	happyIn27
		 (Dummy.Abs.ClassOp_Dec happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_3  8# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut34 happy_x_3 of { (HappyWrap34 happy_var_3) -> 
	happyIn28
		 (Dummy.Abs.ClassOp_Imp happy_var_1 happy_var_3
	)}}

happyReduce_32 = happyReduce 5# 9# happyReduction_32
happyReduction_32 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut20 happy_x_3 of { (HappyWrap20 happy_var_3) -> 
	case happyOut31 happy_x_5 of { (HappyWrap31 happy_var_5) -> 
	happyIn29
		 (Dummy.Abs.Class_Dec happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_33 = happyReduce 5# 10# happyReduction_33
happyReduction_33 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut38 happy_x_3 of { (HappyWrap38 happy_var_3) -> 
	case happyOut32 happy_x_5 of { (HappyWrap32 happy_var_5) -> 
	happyIn30
		 (Dummy.Abs.Inst_Dec happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_34 = happyReduce 9# 10# happyReduction_34
happyReduction_34 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_3 of { (HappyWrap35 happy_var_3) -> 
	case happyOut20 happy_x_6 of { (HappyWrap20 happy_var_6) -> 
	case happyOut38 happy_x_7 of { (HappyWrap38 happy_var_7) -> 
	case happyOut32 happy_x_9 of { (HappyWrap32 happy_var_9) -> 
	happyIn30
		 (Dummy.Abs.Inst_Dec_With_Constraint happy_var_3 happy_var_6 happy_var_7 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_35 = happySpecReduce_0  11# happyReduction_35
happyReduction_35  =  happyIn31
		 ([]
	)

happyReduce_36 = happySpecReduce_2  11# happyReduction_36
happyReduction_36 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	happyIn31
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_37 = happySpecReduce_0  12# happyReduction_37
happyReduction_37  =  happyIn32
		 ([]
	)

happyReduce_38 = happySpecReduce_2  12# happyReduction_38
happyReduction_38 happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
	happyIn32
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_39 = happySpecReduce_1  13# happyReduction_39
happyReduction_39 happy_x_1
	 =  happyIn33
		 (Dummy.Abs.Nil
	)

happyReduce_40 = happySpecReduce_3  13# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut20 happy_x_3 of { (HappyWrap20 happy_var_3) -> 
	happyIn33
		 (Dummy.Abs.Cons happy_var_1 happy_var_3
	)}}

happyReduce_41 = happySpecReduce_3  14# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut34 happy_x_3 of { (HappyWrap34 happy_var_3) -> 
	happyIn34
		 (Dummy.Abs.Ass_Expr happy_var_1 happy_var_3
	)}}

happyReduce_42 = happyReduce 4# 14# happyReduction_42
happyReduction_42 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut34 happy_x_4 of { (HappyWrap34 happy_var_4) -> 
	happyIn34
		 (Dummy.Abs.Abst_Expr happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_43 = happySpecReduce_1  14# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn34
		 (Dummy.Abs.Var_Expr happy_var_1
	)}

happyReduce_44 = happySpecReduce_2  14# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	happyIn34
		 (Dummy.Abs.App_Expr happy_var_1 happy_var_2
	)}}

happyReduce_45 = happySpecReduce_1  14# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut33 happy_x_1 of { (HappyWrap33 happy_var_1) -> 
	happyIn34
		 (Dummy.Abs.List_Expr happy_var_1
	)}

happyReduce_46 = happyReduce 10# 14# happyReduction_46
happyReduction_46 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	case happyOut33 happy_x_4 of { (HappyWrap33 happy_var_4) -> 
	case happyOut34 happy_x_6 of { (HappyWrap34 happy_var_6) -> 
	case happyOut33 happy_x_8 of { (HappyWrap33 happy_var_8) -> 
	case happyOut34 happy_x_10 of { (HappyWrap34 happy_var_10) -> 
	happyIn34
		 (Dummy.Abs.LCase_Expr happy_var_2 happy_var_4 happy_var_6 happy_var_8 happy_var_10
	) `HappyStk` happyRest}}}}}

happyReduce_47 = happySpecReduce_1  14# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn34
		 (Dummy.Abs.True_Expr happy_var_1
	)}

happyReduce_48 = happySpecReduce_1  14# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn34
		 (Dummy.Abs.False_Expr happy_var_1
	)}

happyReduce_49 = happySpecReduce_2  15# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut38 happy_x_2 of { (HappyWrap38 happy_var_2) -> 
	happyIn35
		 (Dummy.Abs.TypeConstraint happy_var_1 happy_var_2
	)}}

happyReduce_50 = happyReduce 5# 16# happyReduction_50
happyReduction_50 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { (HappyWrap37 happy_var_2) -> 
	case happyOut38 happy_x_5 of { (HappyWrap38 happy_var_5) -> 
	happyIn36
		 (Dummy.Abs.OverLoadedType happy_var_2 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_51 = happySpecReduce_0  17# happyReduction_51
happyReduction_51  =  happyIn37
		 ([]
	)

happyReduce_52 = happySpecReduce_1  17# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	happyIn37
		 ((:[]) happy_var_1
	)}

happyReduce_53 = happySpecReduce_3  17# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	case happyOut37 happy_x_3 of { (HappyWrap37 happy_var_3) -> 
	happyIn37
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_54 = happySpecReduce_1  18# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn38
		 (Dummy.Abs.TVar_SType happy_var_1
	)}

happyReduce_55 = happySpecReduce_2  18# happyReduction_55
happyReduction_55 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	happyIn38
		 (Dummy.Abs.TCons_SType happy_var_1 happy_var_2
	)}}

happyReduce_56 = happySpecReduce_1  18# happyReduction_56
happyReduction_56 happy_x_1
	 =  happyIn38
		 (Dummy.Abs.Bool_SType
	)

happyReduce_57 = happySpecReduce_3  18# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut38 happy_x_3 of { (HappyWrap38 happy_var_3) -> 
	happyIn38
		 (Dummy.Abs.Arrow_SType happy_var_1 happy_var_3
	)}}

happyReduce_58 = happySpecReduce_3  18# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_2 of { (HappyWrap38 happy_var_2) -> 
	happyIn38
		 (Dummy.Abs.List_SType happy_var_2
	)}

happyReduce_59 = happySpecReduce_1  19# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	happyIn39
		 (Dummy.Abs.DType_OvType happy_var_1
	)}

happyReduce_60 = happySpecReduce_1  19# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	happyIn39
		 (Dummy.Abs.DType_SType happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 22# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TL happy_dollar_dollar) -> cont 19#;
	PT _ (T_True happy_dollar_dollar) -> cont 20#;
	PT _ (T_False happy_dollar_dollar) -> cont 21#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 22# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap23 x') = happyOut23 x} in x'))

pListClassDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap24 x') = happyOut24 x} in x'))

pListInstDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap25 x') = happyOut25 x} in x'))

pListExpr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap26 x') = happyOut26 x} in x'))

pClassOpDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap27 x') = happyOut27 x} in x'))

pClassOpImp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap28 x') = happyOut28 x} in x'))

pClassDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap29 x') = happyOut29 x} in x'))

pInstDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap30 x') = happyOut30 x} in x'))

pListClassOpDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap31 x') = happyOut31 x} in x'))

pListClassOpImp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap32 x') = happyOut32 x} in x'))

pList tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap33 x') = happyOut33 x} in x'))

pExpr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap34 x') = happyOut34 x} in x'))

pTyC tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap35 x') = happyOut35 x} in x'))

pOvType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap36 x') = happyOut36 x} in x'))

pListTyC tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap37 x') = happyOut37 x} in x'))

pSType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap38 x') = happyOut38 x} in x'))

pDType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap39 x') = happyOut39 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
