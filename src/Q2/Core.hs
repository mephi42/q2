module Q2.Core where

  import Data.Maybe
  import Q2.TH.Replace
  import Language.Haskell.TH



  -- |Concrete address space.
  class (Show a) => As a

  -- |Concrete data format.
  class (Show f) => Format f a



  -- |Integer value.
  data IntVal = IntVal Integer

  -- |Boolean value.
  data BoolVal = BoolVal Bool

  -- |Address space value.
  data AsVal where
    AsVal :: (As a) => a -> AsVal

  -- |Address value.
  data AddrVal = AddrVal AsVal Integer

  -- |Data format value.
  data FormatVal a where
    FormatVal :: (Format f a) => f -> FormatVal a



  -- |Something convertible to address space expression.
  class AsLike a where
    toAs :: a -> Expr AsVal

  instance (As a) => AsLike a where
    toAs x = AsLiteral $ AsVal x

  instance AsLike AsVal where
    toAs x = AsLiteral x



  -- |Something convertible to data format expression.
  class FormatLike f a where
    toFormat :: f -> Expr (FormatVal a)

  instance (Format f a) => FormatLike f a where
    toFormat x = FormatLiteral $ FormatVal x

  instance FormatLike (FormatVal a) a where
    toFormat x = FormatLiteral x



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

    -- |Address space literal.
    AsLiteral :: AsVal -> Expr AsVal

    -- |Address.
    Addr :: Expr AsVal -> Expr IntVal -> Expr AddrVal

    -- |Data format.
    FormatLiteral :: FormatVal v -> Expr (FormatVal v)

    -- |Memory cell.
    Cell :: Expr (FormatVal (Expr v)) -> Expr AddrVal -> Expr v

    -- |Binary operation
    BinOp :: BinOp l r v -> Expr l -> Expr r -> Expr v



  -- |Constructs address.
  (!) :: (AsLike a) => a -> Expr IntVal -> Expr AddrVal
  (!) a x = Addr (toAs a) x

  -- |Constructs cell.
  ptr :: (FormatLike f (Expr v)) => f -> Expr AddrVal -> Expr v
  ptr f a = Cell (toFormat f) a

  -- |Constructs power expression.
  (**) :: Expr IntVal -> Expr IntVal -> Expr IntVal
  (**) x y = BinOp Pow x y



  -- |Decoder.
  data Decoder a where
    -- |Decodes next bits using given format and pushes result.
    Decode :: (FormatLike f a) => f -> Decoder a
    -- |Evaluates expression and pushes result.
    Eval :: a -> Decoder a
    -- |Runs two decoders one after another.
    Seq :: Decoder a0 -> (a0 -> Decoder a) -> Decoder a
    -- |Evaluates expression and branches based on result.
    Switch :: a0 -> [(a0, Decoder a)] -> Decoder a

  -- |Constructs format decoder.
  decode :: (FormatLike f a) => f -> Decoder a
  decode x = Decode x

  -- |Constructs switch.
  switch :: a0 -> [(a0, Decoder a)] -> Decoder a
  switch e ds = Switch e ds

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
