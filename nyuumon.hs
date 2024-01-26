-- nyumon.hs

-- 1.1 ghci（デバッグに便利な対話環境）の使い方

-- ↓行コメント
-- Haskellのプログラムは基本的に、定数の宣言でできている
someconst = 3

nibai = \x -> x * 2

-- 関数はこのように定義することもできる
sanbai x = x * 3



-- 1.2 関数と型

-- ↓これが複数行コメント
{-
kakezan をこんなふうに定義すると、 kakezan 3 は \y -> 3 * y という関数になる
そうしたら、(kakezan 3) 4 は 3 * 4 = 12 になる
-}
kakezan = \x -> (\y -> x * y)

-- これらはいずれも上の kakezan と同じ意味である
kakezan2 = \x y -> x * y 
kakezan3 x y = x * y 

-- 関数適用は左結合で、優先度は最大である
_3x4 = kakezan 3 4

-- 二引数関数・整数・整数を受け取って、計算して返す
keisan f x y = f x y

-- 中置演算子は丸括弧で囲むとふつうの関数のように扱える
_4x5 = (*) 4 5

tashizan = keisan (+)

_9plus8 = tashizan 9 8 -- 17

-- ○○ -> ×× というのは、○○を受け取って××を返す関数の型を表す
square :: Int -> Int
square x = x * x

-- -> は右結合なので、こう書くと Int -> (Int -> Int) と同じ意味である
hikizan  :: Int -> Int -> Int
hikizan  = keisan (-)

_5plus8 :: Int
_5plus8 = tashizan 5 (8 :: Int) -- 式に型注釈を付けることもできる



-- 1.3 パターンマッチ・再帰
collatz :: Int -> Int
collatz x =
  case x `mod` 2 of
  -- x `mod` 2 が 0 の場合
    0 -> x `div` 2
  -- _ は「どんな値でもよい」という意味
    _ -> x * 3 + 1

-- 駐車料金（3時間未満なら一律100円、それ以降は1時間ごとに100円増える）
parkingFee :: Int -> Int
parkingFee minutes =
  case minutes `div` 60 of
--  ↓ caseの式の結果は変数に束縛できる
    h | h < 3     -> 100
      | otherwise -> 100 + (h - 2) * 100
--      ↑ こいつらはガード節。マッチングに条件を追加できる

-- 階乗
factorial :: Int -> Int
factorial n =
  case n of
    0 -> 1
    _ -> n * factorial (n - 1)

-- 関数全体が一個のcase式になるときは、こういう引数でパターンマッチする書き方もできる
factorial2 :: Int -> Int
factorial2 0 = 1
factorial2 n = n * factorial (n - 1)

-- 二引数で再帰してもよい
checkCollatz :: Int -> Int -> Int
checkCollatz x step =
  case collatz x of
    1  -> step
    x' -> checkCollatz x' (step + 1) -- 変数名を書くと、一致した際の値が束縛される
    -- ところで、Haskellでは識別子名の末尾に ' を何個でもつけてよい（中間はだめ）

-- 無限ループ！試さないこと
infinityLoop :: Int -> Int
infinityLoop x = infinityLoop x


-- 部分関数
partialTest :: Int -> Int
partialTest x =
  case x of
    0 -> 0

-- >>> partialTest 5
-- [略]\nyuumon.hs:(101,3)-(102,10): Non-exhaustive patterns in case


-- 1.4 自家製の型・できあいの型・型変数・カインド
-- 型「MyBool」を定義する
data MyBool =
    MyTrue 
  | MyFalse
  deriving Show -- ← ghciで値を表示できるようにするおまじない（後で説明する）

-- MyBoolの値は、MyTrue か MyFalse のどちらかである

-- BoolとIntの組か、IntとIntとIntの組を表す型
data BoolAndInt_Or_IntAndIntAndInt =
      BoolAndInt MyBool Int
    | IntAndInt Int Int Int
  deriving Show

-- 型コンストラクタ名と型名は重複してよい
data WrappedInt = WrappedInt Int
  deriving Show

-- BoolAndInt (:: Bool -> Int -> BoolAndInt_Or_IntAndIntAndInt) や
-- IntAndInt  (:: Int -> Int -> Int -> BoolAndInt_Or_IntAndIntAndInt) は、
-- BoolAndInt_Or_IntAndIntAndInt の値コンストラクタであり、関数のように使える

bandi :: BoolAndInt_Or_IntAndIntAndInt
bandi = BoolAndInt MyTrue 3

reverseOrAdd :: BoolAndInt_Or_IntAndIntAndInt -> Int
reverseOrAdd x =
  case x of
    BoolAndInt MyTrue i  -> -i
    BoolAndInt MyFalse i ->  i
    IntAndInt i j k      -> i + j + k

-- >>> reverseOrAdd bandi
-- -3

-- 整数のリスト
data IntList =
    ILNil
  | ILCons Int IntList
  deriving Show -- ← ghciで値を表示できるようにするおまじない（後で説明する）

ilTest :: IntList
ilTest = ILCons 1 (ILCons 2 (ILCons 3 ILNil))

ilSum :: IntList -> Int
ilSum x =
  case x of
    ILNil -> 0
    ILCons y ys -> y + ilSum ys

-- >>> ilSum ilTest
-- 6


-- 型 a の値たちのリスト
data List a =
    LNil
  | LCons a (List a)
  deriving Show -- ← ghciで値を表示できるようにするおまじない（後で説明する）

lIntTest :: List Int
lIntTest = LCons 1 (LCons 2 (LCons 3 LNil))

lBoolTest :: List MyBool
lBoolTest = LCons MyTrue (LCons MyFalse LNil)

-- newtype は、型コンストラクタが1種類のとき専用のdata宣言のようなもの（dataでもよい）
newtype NonStarTypeVarTest (f :: * -> *) a = NSTVTest (f a)

-- >>> :k NonStarTypeVarTest
-- NonStarTypeVarTest :: (* -> *) -> * -> *

-- ここで、小文字で始まるaはどんな型が入ってもよい「型変数」である
lLength :: List a -> Int
lLength x =
  case x of
    LNil -> 0
    LCons y ys -> 1 + lLength ys

-- 型変数は具体的に宣言することができる
lLength2 :: forall a. List a -> Int
lLength2 x =
  case x of
    LNil -> 0
    LCons y ys -> 1 + lLength ys

-- >>> lIntTest
-- >>> lLength lIntTest
-- LCons 1 (LCons 2 (LCons 3 LNil))
-- 3

-- >>> lBoolTest
-- >>> lLength lBoolTest
-- LCons 1 (LCons 2 (LCons 3 LNil))
-- 2
