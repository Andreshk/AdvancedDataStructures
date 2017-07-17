module TreapLogic where

-- Този модул съдържа реалната логика на treap-а.
-- За по-малко объркване тук Tree е името на структурата
-- и тук са дефинирани всички специални за treap-овете функции.
data Tree a = Empty | Node a Double (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show Empty = "Empty treap."
    show tr    = if size tr > 100
                 then "Treap too large for printing!"
                 else prettyPrint 0 tr

-- За красиво извеждане на дървото на екрана - като "нормалните" типове данни 
prettyPrint :: Show a => Int -> Tree a -> String
prettyPrint _ Empty = ""
prettyPrint pad tr@(Node val pr left right) = 
    replicate pad ' ' ++ peekRoot tr ++ " -> " ++ peekRoot left ++ ", " ++ peekRoot right ++ "\n"
    ++ prettyPrint (pad+2) left
    ++ prettyPrint (pad+2) right
  where peekRoot :: Show a => Tree a -> String
        peekRoot Empty = "#"
        peekRoot (Node val pr _ _) = show val ++ " {" ++ show pr ++ "}"

-- Проверка дали даден treap е празен
empty :: Tree a -> Bool
empty Empty = True
empty _ = False

-- Проверка дали даден treap е валиден:
-- КЪМ МОМЕНТА НЕ РАБОТИ КОРЕКТНО (!)
-- - за всеки възел искаме стойността в левия наследник да е по-малка от тази във възела;
--   аналогично стойността в десния наследник да е по-голяма от тази във възела
--   -> оттам TREE като обикновено двоично наредено дърво, гледайки ключовете (т.е. съдържаните данни)
-- - за всеки възел искаме приоритетите в двата наследника да са по-големи от този във възела
--   -> оттам HEAP като обикновена двоична пирамида, гледайки "приоритетите" (произволно генерираните числа)
-- След всяка операция потребителят може да проверява дали получената структура е валидна.
-- сложност: O(n)
valid :: (Eq a, Ord a) => Tree a -> Bool
valid Empty = True
valid (Node val pr left right) = (empty left  || val > (getValue left)  && pr <= (getPriority left)  && valid left)
                              && (empty right || val < (getValue right) && pr <= (getPriority right) && valid right)
  where getValue :: Tree a -> a
        getValue (Node val _ _ _) = val
        getPriority :: Tree a -> Double
        getPriority (Node _ pr _ _) = pr

-- Балансирането на едно дърво представлява поредица от ротации.
-- При всяка операция insert/remove се налагат ротации само по "пътя"
-- от корена до листото с търсения елемент, т.е. логаритмичен брой пъти.
-- Как се ротира дадено дърво се преценява само според приоритетите в двата
-- директни наследника; като в единия случай функцията се извиква рекурсивно
-- надолу, а в другия се извиква нагоре от back-trackingа на рекурсивната
-- функция insert. С тази функция можем да пренареждаме дървото като избутваме
-- някой елемент до "дъното" на дървото (т.е. в листо) или го караме да "изплува" в корена
-- на дървото. Такова пренареждане не нарушава инвариантата на двоичното наредено дърво (!)
-- сложност: О(lgn) в най-лошия случай, ако флагът е True и функцията се извиква
-- рекурсивно надолу и О(1) иначе (локално балансиране)
rebalance :: Bool -> Tree a -> Tree a
rebalance flag tr@(Node _ _ Empty Empty) = if flag then Empty else tr
rebalance flag tr@(Node _ pr left right)
  | pr <= leftpr && pr <= rightpr = tr
  | leftpr < rightpr && flag      = let (Node v p l r) = rotateRight tr in (Node v p l (rebalance True r))
  | leftpr < rightpr              = rotateRight tr
  | leftpr >= rightpr && flag     = let (Node v p l r) = rotateLeft tr in (Node v p (rebalance True l) r)
  | leftpr >= rightpr             = rotateLeft tr
  where leftpr  = specialPr left
        rightpr = specialPr right
        specialPr :: Tree a -> Double
        specialPr Empty = 2.0
        specialPr (Node _ pr _ _) = pr

-- Лява и дясна ротации на двоично наредено дърво.
-- Няма да се налагат други (невалидни) извиквания.
-- Да сте виждали по-елегантни реализации на тези функции? That's Haskell for you.
rotateLeft :: Tree a -> Tree a
rotateLeft (Node xv xp atr (Node yv yp btr ctr)) = Node yv yp (Node xv xp atr btr) ctr

rotateRight :: Tree a -> Tree a
rotateRight (Node yv yp (Node xv xp atr btr) ctr) = Node xv xp atr (Node yv yp btr ctr)

-- "Private" функция за вмъкване в самото дърво, използва се хитро от split и merge
-- При вмъкване на елемент в дървото първо го добавяме като в обикновено ДНД,
-- след което с ротации той "изплува" нагоре, докато приоритетите образуват
-- валидна пирамида.
-- сложност: O(lgn) очаквана (и най-често), O(n) в най-лошия случай
insert :: (Eq a, Ord a) => a -> Double -> Tree a -> Tree a
insert x newpr Empty = Node x newpr Empty Empty
insert x newpr tr@(Node val pr left right)
  | x < val  = rebalance False $ Node val pr (insert x newpr left) right
  | x > val  = rebalance False $ Node val pr left (insert x newpr right)
  | x == val = if newpr == (-1.0) -- използва се при split
               then rebalance False $ Node x newpr left right
               else tr

-- При премахване на един елемент първо го намираме, след което увеличаваме
-- неговия приоритет и балансираме надолу - с голям приоритет той "потъва"
-- и става листо, което листо после отрязваме.
-- сложност: O(lgn) очаквана (и най-често), O(n) в най-лошия случай 
remove :: (Eq a, Ord a) => a -> Tree a -> Tree a
remove _ Empty = Empty
remove x tr@(Node val pr left right)
    | x < val  = Node val pr (remove x left) right
    | x > val  = Node val pr left (remove x right)
    | x == val = rebalance True $ Node val 2.0 left right

-- Търсенето е стандартно търсене в двоично наредено дърво
-- сложност: O(lgn) очаквана (и най-често), O(n) в най-лошия случай
search :: (Eq a, Ord a) => a -> Tree a -> Bool
search _ Empty = False
search x (Node val _ left right)
    | x < val  = search x left
    | x > val  = search x right
    | x == val = True

-- Сливането на два treap-а изисква най-големия елемент в левия treap
-- да е по-малък от най-малкия елемент в десния treap. Осъществява се
-- с фалшиво добавяне на стойността в корена на едното дърво с максимален приоритет,
-- който след балансиране на дървото се намира в някое листо. Това листо
-- после отрязваме, за да избегнем дублиране на ключовете и да получим валиден treap.
-- сложност: O(lgn) очаквана (и най-често), O(n) в най-лошия случай
merge :: (Eq a, Ord a) => Tree a -> Tree a -> Tree a
merge Empty tr = tr
merge tr Empty = tr
merge tr1@(Node v _ _ _) tr2
    | leftmax >= rightmin = error "Unmergeable treaps!"
    | otherwise           = rebalance True $ Node v 2.0 tr1 tr2 --(!)
  where leftmax  = getExtreme True tr1
        rightmin = getExtreme False tr2

-- getExtreme False взима най-малкия елемент в даден Treap;
-- getExtreme True взима най-големия.
getExtreme :: Bool -> Tree a -> a
getExtreme False (Node v _ Empty _) = v
getExtreme False (Node _ _ left _)  = getExtreme False left
getExtreme True (Node v _ _ Empty) = v
getExtreme True (Node _ _ _ right) = getExtreme True right

-- Разцепване на даден treap на два treap-а така, че в единия да са всички
-- ключове, по-малки от даден ключ х, а в другия да са всички по-големи от x.
-- Тук вкарваме елемента x с минимален приоритет така, че след балансиране
-- той да се озове в корена. Тогава (тъй като е двоично наредено дърво)
-- всички елементи, по-малки от него, са в лявото поддърво, а всички по-големи в дясното.
-- Ако x присъства в дървото преди разцепване, то след това той не присъства
-- в нито едно от двете.
-- сложност: O(lgn) очаквана (и най-често), O(n) в най-лошия случай
split :: (Eq a, Ord a) => a -> Tree a -> (Tree a, Tree a)
split x tr = let (Node _ _ left right) = insert x (-1.0) tr in (left, right)

-- Височина на даден treap
-- сложност: O(n)
height :: Tree a -> Int
height Empty = 0
height (Node _ _ left right) = 1 + max (height left) (height right)

-- Брой елементи в даден treap
-- сложност: O(n)
-- Може да се направи в константно време ако всеки treap носи със себе си 
-- своя размер така, както "носи" и генератора си. Тогава обаче всеки insert/remove
-- ще трябва да връща наредена двойка от новия treap и неговия размер.
size :: Tree a -> Int
size Empty = 0
size (Node _ _ left right) = 1 + (size left) + (size right)

-- Списък от елементите в даден treap, подредени в сортиран ред.
-- "toList t1 ++ toList t2 == (toList $ merge t1 t2)"
-- е изпълнено винаги, когато сливането е позволено, докато
-- "let (l,r) = split x t in toList l ++ toList r == toList t"
-- е изпълнено само когато елементът x не е присъствал предварително в t.
-- сложност: O(n)
toList :: Tree a -> [a]
toList Empty = []
toList (Node val _ left right) = toList left ++ [val] ++ toList right