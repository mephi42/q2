module Q2.Core where

  import Data.Maybe
  import Q2.TH.Replace
  import Language.Haskell.TH

  -- |Integer value.
  data IntVal = IntVal Integer

  -- |Boolean value.
  data BoolVal = BoolVal Bool

  -- |Binary operation type.
  data BinOp l r v where
    -- |Addition.
    Add :: BinOp IntVal IntVal IntVal

    -- |Power.
    Pow :: BinOp IntVal IntVal IntVal

  -- |Expression.
  data Expr v where
    -- |Variable.
    Var :: Int -> Expr v

    -- |Integer literal.
    IntLiteral :: IntVal -> Expr IntVal

    -- |Boolean literal.
    BoolLiteral :: BoolVal -> Expr BoolVal

    -- |Binary operation
    BinOp :: BinOp l r v -> Expr l -> Expr r -> Expr v

  -- |Constructs power expression.
  pow :: Expr IntVal -> Expr IntVal -> Expr IntVal
  pow x y = BinOp Pow x y

  -- |Data format.
  class Format f a

  -- |Decoder.
  data Decoder a where
    -- Decode next bits using given format and push result.
    Decode :: (Format f a) => f -> Decoder a
    -- Evaluate expression and push result.
    Eval :: a -> Decoder a
    -- Run two decoders one after another.
    Seq :: Decoder a0 -> (a0 -> Decoder a) -> Decoder a
    -- Evaluate expression and branch based on result.
    Switch :: a0 -> [(a0, Decoder a)] -> Decoder a

  -- |Constructs format decoder.
  decode :: (Format f a) => f -> Decoder a
  decode x = Decode x

  -- |Decoder chaining.
  instance Monad Decoder where
    return x = Eval x
    x >>= y = Seq x y

  -- |Creates IntLiteral expression.
  mkIntLiteral :: Integer -> Exp
  mkIntLiteral x = AppE (ConE (mkName "IntLiteral")) (AppE (ConE (mkName "IntVal")) (LitE (IntegerL x)))

  -- |Replaces integer literals with IntLiterals.
  fixLit :: ExpReplaceable a => a -> a
  fixLit = fromJust . expReplace (\s -> case s of
                                          LitE (IntegerL x) -> Replace (mkIntLiteral x)
                                          other -> Continue other)

  -- |Applies fixups to DSL.
  q2 :: ExpReplaceable a => Q a -> Q a
  q2 x = x >>= (return . fixLit)
