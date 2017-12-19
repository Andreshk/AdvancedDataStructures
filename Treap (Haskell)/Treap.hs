module Treap
( makeTreap, empty, valid,
  insert, remove, search, merge, split,
  insertList, removeList, height, size,
  fromList, toList, sort
) where

import qualified TreapLogic as TL hiding (Node, prettyprint, rebalance, getExtreme, rotateLeft, rotateRight)
import System.Random (StdGen, mkStdGen, randomR)
import Data.List (foldl')

-- За да запазим принципите на чистото функционално програмиране всеки отделен
-- treap носи със себе си и генератор на произволни числа. При всяко генериране на
-- произволно число той връща наредена двойка от числото и своето новото състояние.
-- Това означава, че при всяко създаване на treap с функциите makeTreap или fromList
-- трябва да подадем и seed на неговия генератор. Макар да изглежда в конфликт с идеята
-- за рандомизирани структури, така гарантираме че при всяко извикване с един и същ
-- seed ще получим едно и също дърво, което все пак ще бъде добре балансирано.

-- Тази структура е обвивка на реалния treap и делегира повечето функционалности
-- на него. За подробно описание и документация вж. TreapLogic.hs, където един
-- имплементирана същинската логика за структурата от данни Treap.
data Treap a = Treap (TL.Tree a) StdGen

instance Show a => Show (Treap a) where
    show (Treap tr _) = show tr

-- Създаване на празен treap по seed на неговия генератор на произволни числа
makeTreap :: Int -> Treap a
makeTreap seed = Treap TL.Empty (mkStdGen seed)

-- Проверка дали даден treap е празен
empty :: Treap a -> Bool
empty (Treap tr _) = TL.empty tr

-- Проверка дали даден treap е валиден
valid :: (Eq a, Ord a) => Treap a -> Bool
valid (Treap tr _) = TL.valid tr

-- Вмъкване на елемент в treap
insert :: (Eq a, Ord a) => a -> Treap a -> Treap a
insert x (Treap tr gen) = Treap newtr newgen
  where (newpr, newgen) = randomR (0.0, 1.0) gen
        newtr = TL.insert x newpr tr

-- Премахване на елемент от treap
remove :: (Eq a, Ord a) => a -> Treap a -> Treap a
remove x (Treap tr gen) = Treap (TL.remove x tr) gen

-- Търсене на елемент в treap
search :: (Eq a, Ord a) => a -> Treap a -> Bool
search x (Treap tr _) = TL.search x tr

-- Сливане на два treap-а
merge :: (Eq a, Ord a) => Treap a -> Treap a -> Treap a
merge (Treap tr1 gen) (Treap tr2 _) = Treap (TL.merge tr1 tr2) gen

-- Разцепване на даден treap на два treap-а така, че в единия да са всички
-- ключове, по-малки от даден ключ х, а в другия да са всички по-големи от x.
split :: (Eq a, Ord a) => a -> Treap a -> (Treap a, Treap a)
split x (Treap tr gen) = (Treap left gen, Treap right gen)
  where (left, right) = TL.split x tr

-- Вмъкване на множество елементи в treap
insertList :: (Eq a, Ord a) => [a] -> Treap a -> Treap a
insertList = flip $ foldl' (flip insert)

-- Премахване на множество елементи от treap
removeList :: (Eq a, Ord a) => [a] -> Treap a -> Treap a
removeList = flip $ foldl' (flip remove)

-- Височина на treap
height :: Treap a -> Int
height (Treap tr _) = TL.height tr

-- Брой съдържани елементи в treap
size :: Treap a -> Int
size (Treap tr _) = TL.size tr

-- Построяване на нов treap по списък от елементи
-- сложност: O(nlgn)
fromList :: (Eq a, Ord a) => Int -> [a] -> Treap a
fromList seed lst = insertList lst (makeTreap seed)

-- Списък от елементите в даден treap, подредени в сортиран ред
toList :: Treap a -> [a]
toList (Treap tr _) = TL.toList tr

-- Сортиране на списък чрез вкарване на всички елементи в treap
-- и извеждането им в сортиран ред. Очевидно O(nlgn) очаквана сложност.
-- Налага се този seed да бъде hardcode-нат.
sort :: (Eq a, Ord a) => [a] -> [a]
sort = toList . fromList 20120618

--iei
