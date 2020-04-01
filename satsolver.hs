import Data.List (delete, find, maximumBy, zipWith, nub, group, sort)
import Data.Maybe (mapMaybe, fromJust)
import Data.Function (on)
import Data.Set (toList, fromList, intersection, difference)
import System.IO

-- TASK 1
data Expression a = T
  | F
  | Not (Expression a)
  | And [Expression a]
  | Or  [Expression a]
  | Variable a deriving (Show, Eq, Ord)


removeConstants exp =
  case exp of
    Not a -> (case (removeConstants a) of
                T -> F
                F -> T
                e -> Not e)
    Or [] -> F
    Or (y:[]) -> y
    Or ys -> (let pomozna x acc = (case acc of
                                     T -> T
                                     Or s -> (let x_new = removeConstants x in
                                              case x_new of
                                                T -> T
                                                F -> Or s
                                                _ -> Or (x_new:s))
                                     _ -> x)
              in
              foldr pomozna (Or []) ys)
    And [] -> T
    And ys -> (let pomozna x acc = (case acc of
                                       F -> F
                                       And s -> (let x_new = removeConstants x in
                                                 case x_new of
                                                   F -> F
                                                   T -> And s
                                                   _ -> And (x_new:s))
                                       _ -> x)
               in
               foldr pomozna (And []) ys)
    v -> v


simplify exp =
  let komp e = removeConstants e in
  case exp of
    Not e -> komp (Not e)
    Or ys -> komp (Or (map simplify ys)) 
    And ys -> komp (And (map simplify ys))
    v -> v

-- (* Find expression e1 in expression exp And replace it with expression e2 *) 
findAndReplace exp e1 e2 =
  case exp of
    Not a -> if e1 == Not a then e2 else Not (findAndReplace a e1 e2)
    Or ys -> Or (map (\y -> findAndReplace y e1 e2) ys)
    And ys -> And (map (\y -> findAndReplace y e1 e2) ys)
    v -> if v == e1 then e2 else v

--(* Returns list of all variables in expression. *)
variablesInExp exp =
    case exp of
      Not a -> variablesInExp a
      Or xs -> concat (foldr (\ a acc -> (variablesInExp a):acc) [] xs)
      And xs -> concat (foldr (\ a acc -> (variablesInExp a):acc) [] xs)
      Variable a -> [a]
      _ -> []

-- (* SAT solver *)
satsolver exp =
  let poenostavi [] exp = simplify exp
      poenostavi ((v, b):r) exp = poenostavi r (simplify (findAndReplace exp (Variable v) (if b then T else F)))
      mostCommon = maximum . map (\x -> (length x, head x)) . group . sort
      onElement e = case e of
                      Variable v -> (v, True)
                      Not (Variable v) -> (v, False)
                      Or [y] -> onElement y
                      _ -> error "expressionIsNotKNO"

      firstStep exp =
        let f e = case e of
                    Variable _ -> True
                    Not _ -> True
                    Or [_] -> True
                    _ -> False
            result s exp = case exp of
                             And ys -> (let new = filter f ys in
                                        if (length new) /= 0
                                        then (let s2 = map onElement new in
                                              result (s++s2) (poenostavi s2 (And ys)))
                                        else (s, exp))
                             T -> (s, exp)
                             F -> (s, exp)
                             _ -> error "expressionIsNotKNO"
            (resultNew, expNew) = result [] exp
        in  
        secondStep resultNew expNew
        
      secondStep r exp = case exp of
                           T -> Just r
                           F -> Nothing
                           And [] -> Just r
                           Or [] -> Nothing
                           _ -> thirdStep (Just r) exp
                           
      thirdStep r exp =
        let mostCommonVar = snd (mostCommon (variablesInExp exp))
            r1 = (mostCommonVar, True):(fromJust r)
            e1 = poenostavi r1 exp
            r2 = (mostCommonVar, False):(fromJust r)
            e2 = poenostavi r2 exp
            stepT = firstStep e1
        in
        case stepT of
          Nothing -> (case (firstStep e2) of
                        Nothing -> Nothing
                        Just a -> Just (r2 ++ a))
          Just a -> Just (a ++ r1)
  in
  firstStep exp

removeDups = map head . group . sort

stringToExp string =
  let list = map (\x -> if read x < 0 then Not $ Variable ("x"++show(abs(read x)))
                        else Variable ("x"++x))(words string)
  in
  if length list == 2 then head list
  else (Or (reverse $ tail $ reverse $ list))
  
solutionToString exp =
  let list = map (\(x, b) -> if b then tail x ++ " "
                             else "-" ++ tail x ++ " ") exp
  in
  list
  

mysolver input output = do
  inputFile <- readFile input
  let list = dropWhile (\x -> take 1 x == "c") (lines inputFile)
  let kno = And (map stringToExp (tail list))
  let solution = satsolver kno
  let content = case solution of
                 Nothing -> "0"
                 Just a -> concat (solutionToString (removeDups a))
  outputFile <- openFile output WriteMode
  hPutStrLn outputFile content
  hClose outputFile
