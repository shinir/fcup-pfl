{- Exercício 4.8
   Somar e multiplicar polinómios representados
  como listas de coeficientes.

  Exemplo
   1+2X-X² ~ [1, 2, -1]
-}

-- somar polinómios; versão usando listas em compreensão
addPoly' :: [Int] -> [Int] -> [Int]
addPoly' ps qs
  | n<=m =  -- qs é maior ou igual que ps
     [p+q | (p,q) <- zip (ps ++ replicate (m-n) 0) qs]
  | otherwise = -- ps é maior que qs
      [p+q | (p,q) <- zip ps (qs ++ replicate (n-m) 0) ]
  where
    n = length ps
    m = length qs

-- somar polinómios: versão usando recursão
addPoly :: [Int] -> [Int] -> [Int]
addPoly (p:ps) (q:qs) = (p+q):addPoly ps qs
addPoly []      qs    = qs
addPoly ps      []    = ps

{-
  Solução sem recursão usando listas em compreensão:
  P = sum_0<=i<=n p_i * X^i
  Q = sum_0<=j<=m q_j * X^j
  Logo P*Q = sum_0<=k<=n+m c_k * X^k
  onde c_k = sum {p_i*q_j | i+j = k}
-}

multPoly' :: [Int] -> [Int] -> [Int]
multPoly' ps qs = [sum [pi*qj | (i,pi)<-zip [0..] ps, (j,qj)<-zip [0..] qs,
                        i+j == k]
                  | k<-[0..kmax]]
                         
  where
    kmax = length ps + length qs - 2 

{-
 Multiplicar polinómios
 Ideia do algoritmo recursivo:
 Se
  P = a0 + X*P'
  Q = b0 + X*Q'
 então
  P*Q = a0*b0 + a0*X*Q' + b0*X*P' + X²*P'*Q'
      = a0*b0 + X*(a0*Q' + b0*P' + X*P'*Q')
 -}

multPoly :: [Int] -> [Int] -> [Int]
multPoly []      qs      = []
multPoly ps      []      = []
multPoly (a0:ps) (b0:qs) = (a0*b0) : restantes
  where
    restantes = addPoly (addPoly resto1 resto2) resto3
    resto1 = [a0*q | q<-qs]
    resto2 = [b0*p | p<-ps]
    resto3 = 0 : multPoly ps qs

