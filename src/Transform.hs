--Transform.hs
--

module Transform where


import Data.Dynamic   -- Typeable class and boilerplate generic functions
import Data.Generics
import Param
import FortranP

-- Generic transformation functions
--
transP :: ProgramP -> ProgramP
transP (P p e) = P Void (gen p e)

transA :: ArgP -> ArgP
transA (A p e) = A Void (gen p e)

transG :: ArgNameP -> ArgNameP
transG (G p e) = G Void (gen p e)

transL :: ArgListP -> ArgListP
transL (L p e) = L Void (gen p e)

transE :: ExprP -> ExprP
transE (E p e) = E Void (gen p e)
  
transF :: FortranP -> FortranP
transF (F p f) = F Void (gen p f)

transT :: TypeP -> TypeP
transT (T p t) = T Void (gen p t)

transD :: DeclP -> DeclP
transD (D p d) = D Void (gen p d)

transB :: BlockP -> BlockP
transB (B p b) = B Void (gen p b)

transS :: SubNameP -> SubNameP
transS (S p e) = S Void (gen p e)

transY :: BaseTypeP -> BaseTypeP
transY (Y p y) = Y Void (gen p y)

genF :: Data g => g -> g
genF = everywhere' (id     `extT`
                    transP `extT`
                    transA `extT`
                    transG `extT`
                    transL `extT`
                    transF `extT`
                    transD `extT`
                    transB `extT`
                    transT `extT`
                    transS `extT`
                    transE `extT` 
                    transY
                   )
