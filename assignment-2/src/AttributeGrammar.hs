

-- UUAGC 0.9.52.2 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

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
    (let _stat'IfinalAG :: ([Int])
         _stat'IflowAG :: ([(Int,Int)])
         _stat'IinitStatAG :: Int
         ( _stat'IfinalAG,_stat'IflowAG,_stat'IinitStatAG) =
             stat'_
     in  ( ))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let
     in  ( ))
sem_Code_CProgram :: (T_Program') ->
                     T_Code
sem_Code_CProgram program'_ =
    (let _program'IfinalAG :: ([Int])
         _program'IflowAG :: ([(Int,Int)])
         _program'IinitStatAG :: Int
         ( _program'IfinalAG,_program'IflowAG,_program'IinitStatAG) =
             program'_
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
                  ({-# LINE 118 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel (_lhsIlabel + 1) name_ inp_ out_ _statIstat
                   {-# LINE 352 "AttributeGrammar.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 119 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 357 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 120 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 362 "AttributeGrammar.hs" #-}
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
    (let _statIfinalAG :: ([Int])
         _statIflowAG :: ([(Int,Int)])
         _statIinitStatAG :: Int
         ( _statIfinalAG,_statIflowAG,_statIinitStatAG) =
             stat_
     in  ( ))
-- Procs -------------------------------------------------------
type Procs = [Proc]
-- cata
sem_Procs :: Procs ->
             T_Procs
sem_Procs list =
    (Prelude.foldr sem_Procs_Cons sem_Procs_Nil (Prelude.map sem_Proc list))
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
sem_Procs_Cons hd_ tl_ =
    (\ _lhsIlabel ->
         (let _lhsOprocs :: Procs'
              _lhsOlabel :: Int
              _hdOlabel :: Int
              _tlOlabel :: Int
              _hdIlabel :: Int
              _hdIproc :: Proc'
              _tlIlabel :: Int
              _tlIprocs :: Procs'
              _lhsOprocs =
                  ({-# LINE 114 "AttributeGrammar.ag" #-}
                   _hdIproc : _tlIprocs
                   {-# LINE 433 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 107 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 438 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 103 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 443 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 107 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 448 "AttributeGrammar.hs" #-}
                   )
              ( _hdIlabel,_hdIproc) =
                  hd_ _hdOlabel
              ( _tlIlabel,_tlIprocs) =
                  tl_ _tlOlabel
          in  ( _lhsOlabel,_lhsOprocs)))
sem_Procs_Nil :: T_Procs
sem_Procs_Nil =
    (\ _lhsIlabel ->
         (let _lhsOprocs :: Procs'
              _lhsOlabel :: Int
              _lhsOprocs =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 463 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 107 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 468 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOprocs)))
-- Procs' ------------------------------------------------------
type Procs' = [Proc']
-- cata
sem_Procs' :: (Procs') ->
              (T_Procs')
sem_Procs' list =
    (Prelude.foldr sem_Procs'_Cons sem_Procs'_Nil (Prelude.map sem_Proc' list))
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
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (let
     in  ( ))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
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
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   Program' _procsIprocs _statIstat
                   {-# LINE 533 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 95 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 538 "AttributeGrammar.hs" #-}
                   )
              _procsOlabel =
                  ({-# LINE 107 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 543 "AttributeGrammar.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 99 "AttributeGrammar.ag" #-}
                   _procsIlabel
                   {-# LINE 548 "AttributeGrammar.hs" #-}
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
type T_Program' = ( ([Int]),([(Int,Int)]),Int)
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {finalAG_Syn_Program' :: ([Int]),flowAG_Syn_Program' :: ([(Int,Int)]),initStatAG_Syn_Program' :: Int}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG) = sem
     in  (Syn_Program' _lhsOfinalAG _lhsOflowAG _lhsOinitStatAG))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ =
    (let _lhsOinitStatAG :: Int
         _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _statIfinalAG :: ([Int])
         _statIflowAG :: ([(Int,Int)])
         _statIinitStatAG :: Int
         _lhsOinitStatAG =
             ({-# LINE 171 "AttributeGrammar.ag" #-}
              _statIinitStatAG
              {-# LINE 586 "AttributeGrammar.hs" #-}
              )
         _lhsOfinalAG =
             ({-# LINE 172 "AttributeGrammar.ag" #-}
              _statIfinalAG
              {-# LINE 591 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 173 "AttributeGrammar.ag" #-}
              _statIflowAG
              {-# LINE 596 "AttributeGrammar.hs" #-}
              )
         ( _statIfinalAG,_statIflowAG,_statIinitStatAG) =
             stat_
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
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
                  ({-# LINE 123 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 661 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 124 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 666 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 126 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Istat _stat2Istat
                   {-# LINE 686 "AttributeGrammar.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 127 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 691 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 128 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 696 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 129 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 701 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 131 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIstat
                   {-# LINE 721 "AttributeGrammar.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 132 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 726 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 133 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 731 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 747 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 136 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 752 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 138 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 765 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 139 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 770 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 141 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 783 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 142 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 788 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 144 "AttributeGrammar.ag" #-}
                   Seq' _stat1Istat _stat2Istat
                   {-# LINE 807 "AttributeGrammar.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 145 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 812 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 146 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 817 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 822 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 149 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 839 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 150 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 844 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 152 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 856 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 153 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 861 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 155 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 874 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 156 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 879 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 158 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 890 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 159 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 895 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOstat)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOstat :: Stat'
              _lhsOlabel :: Int
              _lhsOstat =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 906 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 162 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 911 "AttributeGrammar.hs" #-}
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
type T_Stat' = ( ([Int]),([(Int,Int)]),Int)
data Inh_Stat' = Inh_Stat' {}
data Syn_Stat' = Syn_Stat' {finalAG_Syn_Stat' :: ([Int]),flowAG_Syn_Stat' :: ([(Int,Int)]),initStatAG_Syn_Stat' :: Int}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat') =
    (let ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG) = sem
     in  (Syn_Stat' _lhsOfinalAG _lhsOflowAG _lhsOinitStatAG))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (let _lhsOinitStatAG :: Int
         _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _lhsOinitStatAG =
             ({-# LINE 176 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 974 "AttributeGrammar.hs" #-}
              )
         _lhsOfinalAG =
             ({-# LINE 177 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 979 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 178 "AttributeGrammar.ag" #-}
              []
              {-# LINE 984 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (let _lhsOinitStatAG :: Int
         _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _stat1IfinalAG :: ([Int])
         _stat1IflowAG :: ([(Int,Int)])
         _stat1IinitStatAG :: Int
         _stat2IfinalAG :: ([Int])
         _stat2IflowAG :: ([(Int,Int)])
         _stat2IinitStatAG :: Int
         _lhsOinitStatAG =
             ({-# LINE 180 "AttributeGrammar.ag" #-}
              labelc_
              {-# LINE 1005 "AttributeGrammar.hs" #-}
              )
         _lhsOfinalAG =
             ({-# LINE 181 "AttributeGrammar.ag" #-}
              [labelc_] ++ _stat1IfinalAG ++ _stat2IfinalAG
              {-# LINE 1010 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 182 "AttributeGrammar.ag" #-}
              _stat1IflowAG ++ _stat2IflowAG ++ [(labelc_, _stat1IinitStatAG),(labelc_, _stat2IinitStatAG)]
              {-# LINE 1015 "AttributeGrammar.hs" #-}
              )
         ( _stat1IfinalAG,_stat1IflowAG,_stat1IinitStatAG) =
             stat1_
         ( _stat2IfinalAG,_stat2IflowAG,_stat2IinitStatAG) =
             stat2_
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (let _lhsOinitStatAG :: Int
         _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _statIfinalAG :: ([Int])
         _statIflowAG :: ([(Int,Int)])
         _statIinitStatAG :: Int
         _lhsOinitStatAG =
             ({-# LINE 184 "AttributeGrammar.ag" #-}
              labelc_
              {-# LINE 1036 "AttributeGrammar.hs" #-}
              )
         _lhsOfinalAG =
             ({-# LINE 185 "AttributeGrammar.ag" #-}
              [labelc_]
              {-# LINE 1041 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 186 "AttributeGrammar.ag" #-}
              _statIflowAG ++ [(labelc_ , _statIinitStatAG)] ++ map (\x -> (x,labelc_)) _statIfinalAG
              {-# LINE 1046 "AttributeGrammar.hs" #-}
              )
         ( _statIfinalAG,_statIflowAG,_statIinitStatAG) =
             stat_
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelExit_ name_ params_ out_ =
    (let _lhsOinitStatAG :: Int
         _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _lhsOinitStatAG =
             ({-# LINE 199 "AttributeGrammar.ag" #-}
              labelCall_
              {-# LINE 1064 "AttributeGrammar.hs" #-}
              )
         _lhsOfinalAG =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              [labelExit_]
              {-# LINE 1069 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 167 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Call'.lhs.flowAG"
              {-# LINE 1074 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (let _lhsOinitStatAG :: Int
         _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _lhsOinitStatAG =
             ({-# LINE 188 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1088 "AttributeGrammar.hs" #-}
              )
         _lhsOfinalAG =
             ({-# LINE 189 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1093 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 190 "AttributeGrammar.ag" #-}
              []
              {-# LINE 1098 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (let _lhsOinitStatAG :: Int
         _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _lhsOinitStatAG =
             ({-# LINE 192 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1112 "AttributeGrammar.hs" #-}
              )
         _lhsOfinalAG =
             ({-# LINE 193 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1117 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 194 "AttributeGrammar.ag" #-}
              []
              {-# LINE 1122 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (let _lhsOinitStatAG :: Int
         _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _stat1IfinalAG :: ([Int])
         _stat1IflowAG :: ([(Int,Int)])
         _stat1IinitStatAG :: Int
         _stat2IfinalAG :: ([Int])
         _stat2IflowAG :: ([(Int,Int)])
         _stat2IinitStatAG :: Int
         _lhsOinitStatAG =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _stat1IinitStatAG
              {-# LINE 1141 "AttributeGrammar.hs" #-}
              )
         _lhsOfinalAG =
             ({-# LINE 197 "AttributeGrammar.ag" #-}
              _stat2IfinalAG
              {-# LINE 1146 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 198 "AttributeGrammar.ag" #-}
              _stat1IflowAG ++ _stat2IflowAG ++ map (\x -> (x,_stat2IinitStatAG)) _stat1IfinalAG
              {-# LINE 1151 "AttributeGrammar.hs" #-}
              )
         ( _stat1IfinalAG,_stat1IflowAG,_stat1IinitStatAG) =
             stat1_
         ( _stat2IfinalAG,_stat2IflowAG,_stat2IinitStatAG) =
             stat2_
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (let _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _lhsOinitStatAG :: Int
         _lhsOfinalAG =
             ({-# LINE 166 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Malloc'.lhs.finalAG"
              {-# LINE 1169 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 167 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Malloc'.lhs.flowAG"
              {-# LINE 1174 "AttributeGrammar.hs" #-}
              )
         _lhsOinitStatAG =
             ({-# LINE 165 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Malloc'.lhs.initStatAG"
              {-# LINE 1179 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (let _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _lhsOinitStatAG :: Int
         _lhsOfinalAG =
             ({-# LINE 166 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Free'.lhs.finalAG"
              {-# LINE 1192 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 167 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Free'.lhs.flowAG"
              {-# LINE 1197 "AttributeGrammar.hs" #-}
              )
         _lhsOinitStatAG =
             ({-# LINE 165 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Free'.lhs.initStatAG"
              {-# LINE 1202 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (let _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _lhsOinitStatAG :: Int
         _lhsOfinalAG =
             ({-# LINE 166 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.RefAssign'.lhs.finalAG"
              {-# LINE 1216 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 167 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.RefAssign'.lhs.flowAG"
              {-# LINE 1221 "AttributeGrammar.hs" #-}
              )
         _lhsOinitStatAG =
             ({-# LINE 165 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.RefAssign'.lhs.initStatAG"
              {-# LINE 1226 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (let _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _lhsOinitStatAG :: Int
         _lhsOfinalAG =
             ({-# LINE 166 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Continue'.lhs.finalAG"
              {-# LINE 1238 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 167 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Continue'.lhs.flowAG"
              {-# LINE 1243 "AttributeGrammar.hs" #-}
              )
         _lhsOinitStatAG =
             ({-# LINE 165 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Continue'.lhs.initStatAG"
              {-# LINE 1248 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (let _lhsOfinalAG :: ([Int])
         _lhsOflowAG :: ([(Int,Int)])
         _lhsOinitStatAG :: Int
         _lhsOfinalAG =
             ({-# LINE 166 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Break'.lhs.finalAG"
              {-# LINE 1260 "AttributeGrammar.hs" #-}
              )
         _lhsOflowAG =
             ({-# LINE 167 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Break'.lhs.flowAG"
              {-# LINE 1265 "AttributeGrammar.hs" #-}
              )
         _lhsOinitStatAG =
             ({-# LINE 165 "AttributeGrammar.ag" #-}
              error "missing rule: Stat'.Break'.lhs.initStatAG"
              {-# LINE 1270 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinalAG,_lhsOflowAG,_lhsOinitStatAG))