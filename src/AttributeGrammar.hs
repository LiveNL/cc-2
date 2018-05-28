

-- UUAGC 0.9.42.3 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "./AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 11 "AttributeGrammar.hs" #-}
-- BExpr -------------------------------------------------------
data BExpr = BConst (Bool)
           | BVar (String)
           | LessThan (IExpr) (IExpr)
           | GreaterThan (IExpr) (IExpr)
           | LessEqual (IExpr) (IExpr)
           | GreaterEqual (IExpr) (IExpr)
           | IEqual (IExpr) (IExpr)
           | BEqual (BExpr) (BExpr)
           | And (BExpr) (BExpr)
           | Or (BExpr) (BExpr)
           | Not (BExpr)
           deriving ( Eq,Show)
-- cata
sem_BExpr :: BExpr ->
             T_BExpr
sem_BExpr (BConst _val) =
    (sem_BExpr_BConst _val)
sem_BExpr (BVar _name) =
    (sem_BExpr_BVar _name)
sem_BExpr (LessThan _left _right) =
    (sem_BExpr_LessThan (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (GreaterThan _left _right) =
    (sem_BExpr_GreaterThan (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (LessEqual _left _right) =
    (sem_BExpr_LessEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (GreaterEqual _left _right) =
    (sem_BExpr_GreaterEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (IEqual _left _right) =
    (sem_BExpr_IEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (BEqual _left _right) =
    (sem_BExpr_BEqual (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (And _left _right) =
    (sem_BExpr_And (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (Or _left _right) =
    (sem_BExpr_Or (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (Not _val) =
    (sem_BExpr_Not (sem_BExpr _val))
-- semantic domain
type T_BExpr = ( )
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( ) = sem
     in  (Syn_BExpr))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let
     in  ( ))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let
     in  ( ))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let
     in  ( ))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let
     in  ( ))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let
     in  ( ))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let
     in  ( ))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let
     in  ( ))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let
     in  ( ))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let
     in  ( ))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let
     in  ( ))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let
     in  ( ))
-- Code --------------------------------------------------------
data Code = CBExpr (BExpr)
          | CIExpr (IExpr)
          | CStat (Stat')
          | CProc (Proc')
          | CProgram (Program')
-- cata
sem_Code :: Code ->
            T_Code
sem_Code (CBExpr _bExpr) =
    (sem_Code_CBExpr (sem_BExpr _bExpr))
sem_Code (CIExpr _iExpr) =
    (sem_Code_CIExpr (sem_IExpr _iExpr))
sem_Code (CStat _stat') =
    (sem_Code_CStat (sem_Stat' _stat'))
sem_Code (CProc _proc') =
    (sem_Code_CProc (sem_Proc' _proc'))
sem_Code (CProgram _program') =
    (sem_Code_CProgram (sem_Program' _program'))
-- semantic domain
type T_Code = ( )
data Inh_Code = Inh_Code {}
data Syn_Code = Syn_Code {}
wrap_Code :: T_Code ->
             Inh_Code ->
             Syn_Code
wrap_Code sem (Inh_Code) =
    (let ( ) = sem
     in  (Syn_Code))
sem_Code_CBExpr :: T_BExpr ->
                   T_Code
sem_Code_CBExpr bExpr_ =
    (let
     in  ( ))
sem_Code_CIExpr :: T_IExpr ->
                   T_Code
sem_Code_CIExpr iExpr_ =
    (let
     in  ( ))
sem_Code_CStat :: (T_Stat') ->
                  T_Code
sem_Code_CStat stat'_ =
    (let
     in  ( ))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let
     in  ( ))
sem_Code_CProgram :: (T_Program') ->
                     T_Code
sem_Code_CProgram program'_ =
    (let
     in  ( ))
-- Expr --------------------------------------------------------
data Expr = B (BExpr)
          | I (IExpr)
          deriving ( Eq,Show)
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (B _bExpr) =
    (sem_Expr_B (sem_BExpr _bExpr))
sem_Expr (I _iExpr) =
    (sem_Expr_I (sem_IExpr _iExpr))
-- semantic domain
type T_Expr = ( )
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( ) = sem
     in  (Syn_Expr))
sem_Expr_B :: T_BExpr ->
              T_Expr
sem_Expr_B bExpr_ =
    (let
     in  ( ))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I iExpr_ =
    (let
     in  ( ))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( )
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( ) = sem
     in  (Syn_Exprs))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let
     in  ( ))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let
     in  ( ))
-- IExpr -------------------------------------------------------
data IExpr = IConst (Int)
           | Var (String)
           | Plus (IExpr) (IExpr)
           | Minus (IExpr) (IExpr)
           | Times (IExpr) (IExpr)
           | Divide (IExpr) (IExpr)
           | Deref (IExpr)
           deriving ( Eq,Show)
-- cata
sem_IExpr :: IExpr ->
             T_IExpr
sem_IExpr (IConst _val) =
    (sem_IExpr_IConst _val)
sem_IExpr (Var _name) =
    (sem_IExpr_Var _name)
sem_IExpr (Plus _left _right) =
    (sem_IExpr_Plus (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Minus _left _right) =
    (sem_IExpr_Minus (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Times _left _right) =
    (sem_IExpr_Times (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Divide _left _right) =
    (sem_IExpr_Divide (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Deref _ptr) =
    (sem_IExpr_Deref (sem_IExpr _ptr))
-- semantic domain
type T_IExpr = ( )
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( ) = sem
     in  (Syn_IExpr))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let
     in  ( ))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let
     in  ( ))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let
     in  ( ))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let
     in  ( ))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let
     in  ( ))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let
     in  ( ))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let
     in  ( ))
-- Proc --------------------------------------------------------
data Proc = Proc (String) (([String])) (String) (Stat)
          deriving ( Show)
-- cata
sem_Proc :: Proc ->
            T_Proc
sem_Proc (Proc _name _inp _out _stat) =
    (sem_Proc_Proc _name _inp _out (sem_Stat _stat))
-- semantic domain
type T_Proc = Int ->
              ( Int,Proc')
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Int}
data Syn_Proc = Syn_Proc {label_Syn_Proc :: Int,proc_Syn_Proc :: Proc'}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOproc) = sem _lhsIlabel
     in  (Syn_Proc _lhsOlabel _lhsOproc))
sem_Proc_Proc :: String ->
                 ([String]) ->
                 String ->
                 T_Stat ->
                 T_Proc
sem_Proc_Proc name_ inp_ out_ stat_ =
    (\ _lhsIlabel ->
         (let _lhsOproc :: Proc'
              _statOlabel :: Int
              _lhsOlabel :: Int
              _statIlabel :: Int
              _statIstat :: Stat'
              _lhsOproc =
                  ({-# LINE 127 "./AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel (_lhsIlabel + 1) name_ inp_ out_ _statIstat
                   {-# LINE 344 "AttributeGrammar.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 128 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 349 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 129 "./AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 354 "AttributeGrammar.hs" #-}
                   )
              ( _statIlabel,_statIstat) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOproc)))
-- Proc' -------------------------------------------------------
data Proc' = Proc' (Int) (Int) (String) (([String])) (String) (Stat')
           deriving ( Show)
-- cata
sem_Proc' :: (Proc') ->
             (T_Proc')
sem_Proc' (Proc' _labelEntry _labelReturn _name _inp _out _stat) =
    (sem_Proc'_Proc' _labelEntry _labelReturn _name _inp _out (sem_Stat' _stat))
-- semantic domain
type T_Proc' = ( )
data Inh_Proc' = Inh_Proc' {}
data Syn_Proc' = Syn_Proc' {}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc') =
    (let ( ) = sem
     in  (Syn_Proc'))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelReturn_ name_ inp_ out_ stat_ =
    (let
     in  ( ))
-- Procs -------------------------------------------------------
data Procs = Cons (Proc) (Procs)
           | Nil
           deriving ( Show)
-- cata
sem_Procs :: Procs ->
             T_Procs
sem_Procs (Cons _proc _procs) =
    (sem_Procs_Cons (sem_Proc _proc) (sem_Procs _procs))
sem_Procs (Nil) =
    (sem_Procs_Nil)
-- semantic domain
type T_Procs = Int ->
               ( Int,Procs')
data Inh_Procs = Inh_Procs {label_Inh_Procs :: Int}
data Syn_Procs = Syn_Procs {label_Syn_Procs :: Int,procs_Syn_Procs :: Procs'}
wrap_Procs :: T_Procs ->
              Inh_Procs ->
              Syn_Procs
wrap_Procs sem (Inh_Procs _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOprocs) = sem _lhsIlabel
     in  (Syn_Procs _lhsOlabel _lhsOprocs))
sem_Procs_Cons :: T_Proc ->
                  T_Procs ->
                  T_Procs
sem_Procs_Cons proc_ procs_ =
    (\ _lhsIlabel ->
         (let _lhsOprocs :: Procs'
              _lhsOlabel :: Int
              _procOlabel :: Int
              _procsOlabel :: Int
              _procIlabel :: Int
              _procIproc :: Proc'
              _procsIlabel :: Int
              _procsIprocs :: Procs'
              _lhsOprocs =
                  ({-# LINE 122 "./AttributeGrammar.ag" #-}
                   Cons' _procIproc _procsIprocs
                   {-# LINE 425 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 107 "./AttributeGrammar.ag" #-}
                   _procsIlabel
                   {-# LINE 430 "AttributeGrammar.hs" #-}
                   )
              _procOlabel =
                  ({-# LINE 111 "./AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 435 "AttributeGrammar.hs" #-}
                   )
              _procsOlabel =
                  ({-# LINE 107 "./AttributeGrammar.ag" #-}
                   _procIlabel
                   {-# LINE 440 "AttributeGrammar.hs" #-}
                   )
              ( _procIlabel,_procIproc) =
                  proc_ _procOlabel
              ( _procsIlabel,_procsIprocs) =
                  procs_ _procsOlabel
          in  ( _lhsOlabel,_lhsOprocs)))
sem_Procs_Nil :: T_Procs
sem_Procs_Nil =
    (\ _lhsIlabel ->
         (let _lhsOprocs :: Procs'
              _lhsOlabel :: Int
              _lhsOprocs =
                  ({-# LINE 123 "./AttributeGrammar.ag" #-}
                   Nil'
                   {-# LINE 455 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 107 "./AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 460 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOprocs)))
-- Procs' ------------------------------------------------------
data Procs' = Cons' (Proc') (Procs')
            | Nil'
            deriving ( Show)
-- cata
sem_Procs' :: (Procs') ->
              (T_Procs')
sem_Procs' (Cons' _proc _procs) =
    (sem_Procs'_Cons' (sem_Proc' _proc) (sem_Procs' _procs))
sem_Procs' (Nil') =
    (sem_Procs'_Nil')
-- semantic domain
type T_Procs' = ( )
data Inh_Procs' = Inh_Procs' {}
data Syn_Procs' = Syn_Procs' {}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs') =
    (let ( ) = sem
     in  (Syn_Procs'))
sem_Procs'_Cons' :: (T_Proc') ->
                    (T_Procs') ->
                    (T_Procs')
sem_Procs'_Cons' proc_ procs_ =
    (let
     in  ( ))
sem_Procs'_Nil' :: (T_Procs')
sem_Procs'_Nil' =
    (let
     in  ( ))
-- Program -----------------------------------------------------
data Program = Program (Procs) (Stat)
             deriving ( Show)
-- cata
sem_Program :: Program ->
               T_Program
sem_Program (Program _procs _stat) =
    (sem_Program_Program (sem_Procs _procs) (sem_Stat _stat))
-- semantic domain
type T_Program = Int ->
                 ( Int,Program')
data Inh_Program = Inh_Program {label_Inh_Program :: Int}
data Syn_Program = Syn_Program {label_Syn_Program :: Int,program_Syn_Program :: Program'}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program sem (Inh_Program _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOprogram) = sem _lhsIlabel
     in  (Syn_Program _lhsOlabel _lhsOprogram))
sem_Program_Program :: T_Procs ->
                       T_Stat ->
                       T_Program
sem_Program_Program procs_ stat_ =
    (\ _lhsIlabel ->
         (let _lhsOprogram :: Program'
              _lhsOlabel :: Int
              _procsOlabel :: Int
              _statOlabel :: Int
              _procsIlabel :: Int
              _procsIprocs :: Procs'
              _statIlabel :: Int
              _statIstat :: Stat'
              _lhsOprogram =
                  ({-# LINE 119 "./AttributeGrammar.ag" #-}
                   Program' _procsIprocs _statIstat
                   {-# LINE 529 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 103 "./AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 534 "AttributeGrammar.hs" #-}
                   )
              _procsOlabel =
                  ({-# LINE 107 "./AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 539 "AttributeGrammar.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 115 "./AttributeGrammar.ag" #-}
                   _procsIlabel
                   {-# LINE 544 "AttributeGrammar.hs" #-}
                   )
              ( _procsIlabel,_procsIprocs) =
                  procs_ _procsOlabel
              ( _statIlabel,_statIstat) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOprogram)))
-- Program' ----------------------------------------------------
data Program' = Program' (Procs') (Stat')
              deriving ( Show)
-- cata
sem_Program' :: (Program') ->
                (T_Program')
sem_Program' (Program' _procs _stat) =
    (sem_Program'_Program' (sem_Procs' _procs) (sem_Stat' _stat))
-- semantic domain
type T_Program' = ( )
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( ) = sem
     in  (Syn_Program'))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ =
    (let
     in  ( ))
-- Stat --------------------------------------------------------
data Stat = Skip
          | IfThenElse (BExpr) (Stat) (Stat)
          | While (BExpr) (Stat)
          | Call (String) (Exprs) (String)
          | IAssign (String) (IExpr)
          | BAssign (String) (BExpr)
          | Seq (Stat) (Stat)
          | Malloc (String) (IExpr)
          | Free (IExpr)
          | RefAssign (IExpr) (IExpr)
          | Continue
          | Break
          deriving ( Show)
-- cata
sem_Stat :: Stat ->
            T_Stat
sem_Stat (Skip) =
    (sem_Stat_Skip)
sem_Stat (IfThenElse _cond _stat1 _stat2) =
    (sem_Stat_IfThenElse _cond (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (While _cond _stat) =
    (sem_Stat_While _cond (sem_Stat _stat))
sem_Stat (Call _name _params _out) =
    (sem_Stat_Call _name _params _out)
sem_Stat (IAssign _name _val) =
    (sem_Stat_IAssign _name _val)
sem_Stat (BAssign _name _val) =
    (sem_Stat_BAssign _name _val)
sem_Stat (Seq _stat1 _stat2) =
    (sem_Stat_Seq (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (Malloc _name _size) =
    (sem_Stat_Malloc _name _size)
sem_Stat (Free _ptr) =
    (sem_Stat_Free _ptr)
sem_Stat (RefAssign _ptr _val) =
    (sem_Stat_RefAssign _ptr _val)
sem_Stat (Continue) =
    (sem_Stat_Continue)
sem_Stat (Break) =
    (sem_Stat_Break)
-- semantic domain
type T_Stat = Int ->
              ( Int,Stat')
data Inh_Stat = Inh_Stat {label_Inh_Stat :: Int}
data Syn_Stat = Syn_Stat {label_Syn_Stat :: Int,stat_Syn_Stat :: Stat'}
wrap_Stat :: T_Stat ->
             Inh_Stat ->
             Syn_Stat
wrap_Stat sem (Inh_Stat _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOstat) = sem _lhsIlabel
     in  (Syn_Stat _lhsOlabel _lhsOstat))
sem_Stat_Skip :: T_Stat
sem_Stat_Skip =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 132 "./AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 635 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 133 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 640 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_IfThenElse :: BExpr ->
                       T_Stat ->
                       T_Stat ->
                       T_Stat
sem_Stat_IfThenElse cond_ stat1_ stat2_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _stat1Olabel :: Int
              _stat2Olabel :: Int
              _lhsOlabel :: Int
              _stat1Ilabel :: Int
              _stat1Istat :: Stat'
              _stat2Ilabel :: Int
              _stat2Istat :: Stat'
              _lhsOstat =
                  ({-# LINE 134 "./AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Istat _stat2Istat
                   {-# LINE 660 "AttributeGrammar.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 135 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 665 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 136 "./AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 670 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 137 "./AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 675 "AttributeGrammar.hs" #-}
                   )
              ( _stat1Ilabel,_stat1Istat) =
                  stat1_ _stat1Olabel
              ( _stat2Ilabel,_stat2Istat) =
                  stat2_ _stat2Olabel
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_While :: BExpr ->
                  T_Stat ->
                  T_Stat
sem_Stat_While cond_ stat_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _statOlabel :: Int
              _lhsOlabel :: Int
              _statIlabel :: Int
              _statIstat :: Stat'
              _lhsOstat =
                  ({-# LINE 138 "./AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIstat
                   {-# LINE 695 "AttributeGrammar.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 139 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 700 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 140 "./AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 705 "AttributeGrammar.hs" #-}
                   )
              ( _statIlabel,_statIstat) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_Call :: String ->
                 Exprs ->
                 String ->
                 T_Stat
sem_Stat_Call name_ params_ out_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 141 "./AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 721 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 142 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 726 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_IAssign :: String ->
                    IExpr ->
                    T_Stat
sem_Stat_IAssign name_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 143 "./AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 739 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 144 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 744 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_BAssign :: String ->
                    BExpr ->
                    T_Stat
sem_Stat_BAssign name_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 145 "./AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 757 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 146 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 762 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_Seq :: T_Stat ->
                T_Stat ->
                T_Stat
sem_Stat_Seq stat1_ stat2_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _stat1Olabel :: Int
              _stat2Olabel :: Int
              _lhsOlabel :: Int
              _stat1Ilabel :: Int
              _stat1Istat :: Stat'
              _stat2Ilabel :: Int
              _stat2Istat :: Stat'
              _lhsOstat =
                  ({-# LINE 147 "./AttributeGrammar.ag" #-}
                   Seq' _stat1Istat _stat2Istat
                   {-# LINE 781 "AttributeGrammar.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 148 "./AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 786 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 149 "./AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 791 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 150 "./AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 796 "AttributeGrammar.hs" #-}
                   )
              ( _stat1Ilabel,_stat1Istat) =
                  stat1_ _stat1Olabel
              ( _stat2Ilabel,_stat2Istat) =
                  stat2_ _stat2Olabel
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_Malloc :: String ->
                   IExpr ->
                   T_Stat
sem_Stat_Malloc name_ size_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 151 "./AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 813 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 152 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 818 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 153 "./AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 830 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 154 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 835 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_RefAssign :: IExpr ->
                      IExpr ->
                      T_Stat
sem_Stat_RefAssign ptr_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 155 "./AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 848 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 156 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 853 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 157 "./AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 864 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 158 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 869 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 159 "./AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 880 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 160 "./AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 885 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
-- Stat' -------------------------------------------------------
data Stat' = Skip' (Int)
           | IfThenElse' (Int) (BExpr) (Stat') (Stat')
           | While' (Int) (BExpr) (Stat')
           | Call' (Int) (Int) (String) (Exprs) (String)
           | IAssign' (Int) (String) (IExpr)
           | BAssign' (Int) (String) (BExpr)
           | Seq' (Stat') (Stat')
           | Malloc' (Int) (String) (IExpr)
           | Free' (Int) (IExpr)
           | RefAssign' (Int) (IExpr) (IExpr)
           | Continue' (Int)
           | Break' (Int)
           deriving ( Show)
-- cata
sem_Stat' :: (Stat') ->
             (T_Stat')
sem_Stat' (Skip' _label) =
    (sem_Stat'_Skip' _label)
sem_Stat' (IfThenElse' _labelc _cond _stat1 _stat2) =
    (sem_Stat'_IfThenElse' _labelc (sem_BExpr _cond) (sem_Stat' _stat1) (sem_Stat' _stat2))
sem_Stat' (While' _labelc _cond _stat) =
    (sem_Stat'_While' _labelc (sem_BExpr _cond) (sem_Stat' _stat))
sem_Stat' (Call' _labelCall _labelExit _name _params _out) =
    (sem_Stat'_Call' _labelCall _labelExit _name (sem_Exprs _params) _out)
sem_Stat' (IAssign' _label _name _val) =
    (sem_Stat'_IAssign' _label _name (sem_IExpr _val))
sem_Stat' (BAssign' _label _name _val) =
    (sem_Stat'_BAssign' _label _name (sem_BExpr _val))
sem_Stat' (Seq' _stat1 _stat2) =
    (sem_Stat'_Seq' (sem_Stat' _stat1) (sem_Stat' _stat2))
sem_Stat' (Malloc' _label _name _size) =
    (sem_Stat'_Malloc' _label _name (sem_IExpr _size))
sem_Stat' (Free' _label _ptr) =
    (sem_Stat'_Free' _label (sem_IExpr _ptr))
sem_Stat' (RefAssign' _label _ptr _val) =
    (sem_Stat'_RefAssign' _label (sem_IExpr _ptr) (sem_IExpr _val))
sem_Stat' (Continue' _label) =
    (sem_Stat'_Continue' _label)
sem_Stat' (Break' _label) =
    (sem_Stat'_Break' _label)
-- semantic domain
type T_Stat' = ( )
data Inh_Stat' = Inh_Stat' {}
data Syn_Stat' = Syn_Stat' {}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat') =
    (let ( ) = sem
     in  (Syn_Stat'))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (let
     in  ( ))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (let
     in  ( ))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (let
     in  ( ))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelExit_ name_ params_ out_ =
    (let
     in  ( ))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (let
     in  ( ))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (let
     in  ( ))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (let
     in  ( ))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (let
     in  ( ))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (let
     in  ( ))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (let
     in  ( ))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (let
     in  ( ))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (let
     in  ( ))