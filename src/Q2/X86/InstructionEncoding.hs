module Q2.X86.InstructionEncoding where

  import Q2.Core

  q2 [d|
    -- |Unsigned little-endian integer.
    data UL = UL (Expr IntVal)

    -- |UL is data format.
    instance Format UL (Expr IntVal)

    -- |Creates UL.
    ul :: Expr IntVal -> UL
    ul x = UL x

    -- IA-32 scale.
    scale :: Decoder (Expr IntVal)
    scale = do raw <- decode (ul 3)
               return (pow 2 raw)
    |]
