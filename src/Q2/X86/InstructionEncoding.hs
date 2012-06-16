module Q2.X86.InstructionEncoding where

  import Prelude hiding ((**))
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

    -- |Unsigned byte (8 bits).
    byte :: UL
    byte = UL 8

    -- |Unsigned word (16 bits).
    word :: UL
    word = UL 16

    -- |Unsigned double word (32 bits).
    dword :: UL
    dword = UL 32



    -- |EAX.
    eax :: Expr IntVal
    eax = dword `ptr` (XAX ! 0)

    -- |AX.
    ax :: Expr IntVal
    ax = word `ptr` (XAX ! 0)

    -- |AH.
    ah :: Expr IntVal
    ah = byte `ptr` (XAX ! 8)

    -- |AL.
    al :: Expr IntVal
    al = byte `ptr` (XAX ! 0)



    -- IA-32 scale.
    scale :: Decoder (Expr IntVal)
    scale = do raw <- decode $ UL 3
               return $ 2 ** raw
    |]
