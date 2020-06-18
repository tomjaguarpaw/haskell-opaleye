{-# LANGUAGE Arrows #-}

module Bug where

type T62 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 = (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, (a29, (a30, (a31, (a32, (a33, (a34, (a35, (a36, (a37, (a38, (a39, (a40, (a41, (a42, (a43, (a44, (a45, (a46, (a47, (a48, (a49, (a50, (a51, (a52, (a53, (a54, (a55, (a56, (a57, (a58, (a59, (a60, a61)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

pT62 :: T62 (p a0 b0) (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4) (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8) (p a9 b9) (p a10 b10) (p a11 b11) (p a12 b12) (p a13 b13) (p a14 b14) (p a15 b15) (p a16 b16) (p a17 b17) (p a18 b18) (p a19 b19) (p a20 b20) (p a21 b21) (p a22 b22) (p a23 b23) (p a24 b24) (p a25 b25) (p a26 b26) (p a27 b27) (p a28 b28) (p a29 b29) (p a30 b30) (p a31 b31) (p a32 b32) (p a33 b33) (p a34 b34) (p a35 b35) (p a36 b36) (p a37 b37) (p a38 b38) (p a39 b39) (p a40 b40) (p a41 b41) (p a42 b42) (p a43 b43) (p a44 b44) (p a45 b45) (p a46 b46) (p a47 b47) (p a48 b48) (p a49 b49) (p a50 b50) (p a51 b51) (p a52 b52) (p a53 b53) (p a54 b54) (p a55 b55) (p a56 b56) (p a57 b57) (p a58 b58) (p a59 b59) (p a60 b60) (p a61 b61) -> p (T62 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61) (T62 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34 b35 b36 b37 b38 b39 b40 b41 b42 b43 b44 b45 b46 b47 b48 b49 b50 b51 b52 b53 b54 b55 b56 b57 b58 b59 b60 b61)
pT62 = undefined

chooseChoice :: (a -> i) -> f i -> f a
chooseChoice choose fi = _ $ proc a -> do
  case choose a of
    i  -> fi -< i
