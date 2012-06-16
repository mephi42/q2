module Q2.X86.InstructionEncoding where

  import Q2.Core

  q2 [d|
    -- |General-purpose registers.
    data GPR = XAX
             | XBX
             | XCX
             | XDX
             | XSI
             | XDI
             | XBP
             | XSP
     deriving Show

    instance As GPR

    -- |Control registers.
    data CTL = XFLAGS
             | XIP
      deriving Show

    instance As CTL

    -- |Segment registers.
    data SEG = CS
             | DS
             | ES
             | FS
             | GS
             | SS
      deriving Show

    instance As SEG

    -- |Unsigned little-endian integer.
    data UL = UL (Expr IntVal)

    instance Show UL where
      show (UL _) = "UL"

    instance Format UL (Expr IntVal)

    -- |EAX.
    eax :: Expr IntVal
    eax = cell (UL 32) (addr XAX 0)

    -- IA-32 scale.
    scale :: Decoder (Expr IntVal)
    scale = do raw <- decode $ UL 3
               return $ 2 `pow` raw
    |]
