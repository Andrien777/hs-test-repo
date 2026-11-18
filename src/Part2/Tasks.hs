module Part2.Tasks where

import Util (notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term
  = IntConstant {intValue :: Int} -- числовая константа
  | Variable {varName :: String} -- переменная
  | BinaryTerm {op :: BinaryOp, lhv :: Term, rhv :: Term} -- бинарная операция
  deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) lhs rhs = BinaryTerm Plus lhs rhs

infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) lhs rhs = BinaryTerm Minus lhs rhs

infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) lhs rhs = BinaryTerm Times lhs rhs

infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant val) = IntConstant val
replaceVar varName replacement (Variable name) = if name == varName then replacement else Variable name
replaceVar varName replacement (BinaryTerm op lhs rhs) = BinaryTerm op (replaceVar varName replacement lhs) (replaceVar varName replacement rhs)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluatingOp :: Term -> Term -> BinaryOp -> Term
evaluatingOp (IntConstant val1) (IntConstant val2) op
  | op == Plus = IntConstant (val1 + val2)
  | op == Minus = IntConstant (val1 - val2)
  | op == Times = IntConstant (val1 * val2)
evaluatingOp lhs rhs op = BinaryTerm op lhs rhs

evaluate :: Term -> Term
evaluate (IntConstant value) = IntConstant value
evaluate (Variable name) = Variable name
evaluate (BinaryTerm op lhs rhs) = evaluatingOp (evaluate lhs) (evaluate rhs) op
