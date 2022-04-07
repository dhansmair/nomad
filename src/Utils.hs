module Utils where

import Definitions ( Expr(..) )


makeApp :: Expr -> [Expr] -> Expr
makeApp = foldl App

makeAbs :: [String] -> Expr -> Expr 
makeAbs params ex = foldr Abs ex params
-- makeAbs [] ex = ex
-- makeAbs (x:xs) ex = Abs x (makeAbs xs ex)


-- decompress :: Expr -> Expr
-- decompress (Abs (p1:p2:ps) ex) = Abs [p1] (Abs (p2:ps) (decompress ex))
-- decompress (Abs [p] ex) = Abs [p] ex
-- decompress (Abs [] ex) = ex
-- decompress (App ex args) = App (decompress ex) (map decompress args)
-- decompress (BinOp op a b) = BinOp op (decompress a) (decompress b)
-- decompress other = other

-- compress :: Expr -> Expr
-- compress (Abs ps1 (Abs ps2 ex)) = compress $ Abs (ps1 ++ ps2) ex
-- compress (App ex args) = App (compress ex) (map compress args)
-- compress (BinOp op a b) = BinOp op (compress a) (compress b)
-- compress other = other
