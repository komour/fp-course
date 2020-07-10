# Haskell: ДЗ 0

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/komour/fp-course/blob/master/hw0/LICENSE)

## Срок сдачи задачи
28.02.2020

### Задание 1.
Заселить следующие типы
(формально `undefined` тоже считается заселением типа (причем любого),
но в решении ожидается что-то отличное от `undefined`):

```haskell
distributivity
    :: Either a (b, c)
    -> (Either a b, Either a c)
distributivity = undefined
```

```haskell
associator
    :: (a, (b, c))
    -> ((a, b), c)
associator = undefined
```

```haskell
{-# LANGUAGE TypeOperators #-}

type (<->) a b = (a -> b, b -> a)

eitherAssoc
    :: Either a (Either b c)
    <-> Either (Either a b) c
eitherAssoc = undefined
```

### Задание 2
Заселить среди данных типов те, которые возможно.


```haskell
import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg = undefined

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = undefined

pierce :: ((a -> b) -> a) -> a
pierce = undefined

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = undefined
```

### Задание 3
Предположим у нас есть функция:

```haskell
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)
```

Реализовать с помощью `s` и `const` следующие функции:

```haskell
composition :: (b -> c) -> (a -> b) -> a -> c
composition = undefined
```


(должна вести себя аналогично оператору `(.)` из стандартной библиотеки)

```haskell
identity :: a -> a
identity = undefined
```

(должна вести себя аналогично тождественной функции `id`, определенной в Prelude)

```haskell
contraction :: (a -> a -> b) -> a -> b
contraction = undefined
```

```haskell
permutation :: (a -> b -> c) -> b -> a -> c
permutation = undefined
```

### Задание 4
В модуле `Data.Function` определена функция `fix`, которая является аналогом комбинатора неподвижной точки:

```haskell
fix :: (a -> a) -> a
```

Реализовать с помощью `fix` следующие функции:

```haskell
iterateElement :: a -> [a]
iterateElement = undefined
```
Данная функция должна удовлетворять равенству:
`iterateElement x == [x, x..]`


```haskell
fibonacci :: Integer -> Integer
fibonacci = undefined
```

```haskell
factorial :: Integer -> Integer
factorial = undefined
```

```haskell
mapFix :: (a -> b) -> [a] -> [b]
mapFix = undefined
```


### Задание 5
Как вы знаете из курса по теории типов, в лямбда-исчисления можно кодировать натуральные числа и арифметические операции над ними.

Определим тип нумералов Черча следующим образом:
```haskell
type Nat a = (a -> a) -> a -> a
```

Определим нуль:
```haskell
zero :: Nat a
zero f x = x
```

Определить функцию-последователя (он же инкремент):
```haskell
succChurch :: Nat a -> Nat a
succChurch = undefined
```

Реализовать арифметические операции (сложение и умножение):
```haskell
churchPlus, churchMult
    :: Nat a -> Nat a -> Nat a
churchPlus = undefined
churchMult = undefined
```

Реализовать функцию, которая по нумералу Черча сопоставляет обычное целое число:

```haskell
churchToInt :: Nat Integer -> Integer
churchToInt = undefined
```

Данная функция должна удовлетворять следующим равенствам:
1. `churchToInt zero = 0`
2. `churchToInt (succChurch number) = 1 + churchToInt number`
3. `churchToInt (churchPlus m n) = churchToInt m + churchToInt n`
4. `churchToInt (churchMult m n) = churchToInt m * churchToInt n`

### Задание 6
Определить слабую головную нормальную форму:

```haskell
distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
```

```haskell
import Data.Maybe (mapMaybe)

null $ mapMaybe foo "pole chudes ochen' chudesno"
```
где
```haskell
foo :: Char -> Maybe Double
foo char =
    case char == 'o' of
      True -> Just $ exp pi
      False -> Nothing

-- not actually, but good enough for this task
null :: [a] -> Bool
null [] = True
null _  = False

mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs
```