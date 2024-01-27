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

data Option a =
    None
  | Some a
  deriving Show

-- リストの先頭を返す（失敗することもある）
lHead :: List a -> Option a
lHead x =
  case x of
    LNil -> None
    LCons y ys -> Some y

-- Maybe は、先ほど定義した Option と同じもの
lHead2 :: List a -> Maybe a
lHead2 x =
  case x of
    LNil -> Nothing
    LCons y ys -> Just y

-- a のリストは [a] という型で表される
-- (:) はLCons、[] はLNilに対応する
li2 :: [Int]
li2 = 1 : 2 : 3 : []

-- リストリテラル
li3 :: [MyBool]
li3 = [MyTrue, MyFalse]

-- () は、唯一の値として () を持つ型。2.1節以降で出てくる Functor などと組み合わせて使われることが多い。　() は以下と等価：
data Unit = Unit

theOnlyOneValueOfUnit :: ()
theOnlyOneValueOfUnit = ()



-- 1.5 型クラス
-- [elem] と相互変換できるものたちの型クラス
class Listish elem listish where
  toList :: listish -> [elem]
  fromList :: [elem] -> listish

-- 型クラスの引数を埋めると、Constraintという型クラス制約を表すKindが出てくる
-- >>> :k Listish
-- Listish :: * -> * -> Constraint

-- IntList と [Int] は相互変換できるので、Listishに属させてみる
instance Listish Int IntList where
  toList x =
    case x of
      ILNil -> []
      ILCons y ys -> y : toList ys
  fromList x =
    case x of
      [] -> ILNil
      y : ys -> ILCons y (fromList ys) 

instance Listish a [a] where
  toList x = x
  fromList x = x

instance Listish a (List a) where
  toList x =
    case x of
      LNil -> []
      LCons y ys -> y : toList ys
  fromList x =
    case x of
      [] -> LNil
      y : ys -> LCons y (fromList ys)

listishHead :: Listish elem listish => listish -> Maybe elem
listishHead x =
  case toList x of
    [] -> Nothing
    y : _ -> Just y

-- 型クラス制約を複数書く場合は、次のようにする
listishSum :: (Listish elem listish, Num elem) => listish -> elem
listishSum x =
  case toList x of
    [] -> 0
    y : ys -> y + listishSum ys -- (+) :: Num a => a -> a -> a

data Stars = Stars Int
instance Show Stars where
  show (Stars n) = 
    case n of
      0 -> "" -- ['']と同義
      _ -> '*' : show (Stars (n - 1))


-- 1.6 便利なサイト・関数・構文
-- セクション：中置演算子の片方にだけ値を突っ込んで、関数にする

_minus_5 :: Int -> Int
_minus_5 = (-) 5

_7minus5 :: Int
_7minus5 = _minus_5 7 -- 2

-- 便利な関数

-- 関数合成
-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- リストのmap
-- >>> :t map
-- map :: (a -> b) -> [a] -> [b]

-- リストのfilter
-- >>> :t filter
-- filter :: (a -> Bool) -> [a] -> [a]

-- 試してみる
squareThenFilterUnder30 :: [Int] -> [Int]
squareThenFilterUnder30 = filter (< 30) . map (^ 2)


-- let式：ローカル変数を定義する
squareThenFilterUnder30' :: [Int] -> [Int]
squareThenFilterUnder30' x =
  let
    square        = map (^ 2)
    filterUnder30 = filter (< 30)

    squared = square x
    filtered = filterUnder30 squared
  in
    filtered

-- レコード構文：data宣言の際、各フィールドを取り出すための関数を定義する
data Person = Person
  { name :: String
  , age  :: Int
  }

-- レコード構文を用いて定義した型は、初期化時にフィールド名で値を与えることができる
alice :: Person
alice = Person { age = 20, name = "Alice"}

-- レコード構文を用いて定義した型の値を部分的に書き換える構文もある
aliseOneYearLater :: Person
aliseOneYearLater = alice { age = age alice + 1 }


-- 2.2 mapができるもの、Functor

-- 参考: map :: (a -> b) -> [a] -> [b] 
listmap :: (a -> b) -> List a -> List b
listmap f x =
  case x of
    LNil -> LNil
    LCons y ys -> LCons (f y) (listmap f ys)

maymap :: (a -> b) -> Maybe a -> Maybe b
maymap f x =
  case x of
    Nothing -> Nothing
    Just y -> Just (f y)

-- このような IntTo について「型aをもつ中身」を考えるのは無理そうである
data IntTo a = IntTo (Int -> a)

instance Functor IntTo where
  fmap f (IntTo g) = IntTo (\x -> f (g x)) -- が、実際このfmapはFunctorの則を満たしている

-- 極端な例として、型変数にかかわらず一種類の値しか持たない Phantom という Functor を考えてみる
data Phantom a = Phantom

instance Functor Phantom where
  fmap f Phantom = Phantom -- このfmapもFunctorの則を満たしている

doubleAll :: [Int] -> [Int]
doubleAll x = (\y -> y * 2) <$> x -- <$> は fmap の中置演算子版。めっちゃ便利

-- Maybeの場合、pure・<*> (別名ap) は次のような中身である
pureMaybe :: a -> Maybe a
pureMaybe x = Just x

apMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe mf mx =
  case mf of
    Nothing -> Nothing
    Just f ->
      case mx of
        Nothing -> Nothing
        Just x -> Just (f x)

-- >>> (*) <$> Just 6 <*> Just 7
-- Just 42

-- []の場合、pure・<*> は次のような中身である
pureList :: a -> [a]
pureList x = [x]

apList :: [a -> b] -> [a] -> [b]
apList fs xs =
  case fs of
    [] -> []
    f : fs' -> (f <$> xs) ++ (fs' <*> xs)

-- >>> (*) <$> [1,2,3] <*> [1,10,100]
-- [1,10,100,2,20,200,3,30,300]



-- 2.2 連鎖ができるもの、Monad

-- リストのn番目があれば返す（中置演算子はその名を括弧でくくって定義できる）
(!?) :: [a] -> Int -> Maybe a
(!?) x n =
  case x of
    [] -> Nothing
    y : ys ->
      case n of
        0 -> Just y
        _ | n < 0 -> Nothing
          | otherwise -> (!?) ys (n - 1)

-- indirectTwice l x という、リストlの「リストlのx番目の数字」番目を返す関数を作りたい
-- 例：indirectTwice [2, 0, 1] 1 = Just 2（[2, 0, 1]の1番目は0なので、[2, 0, 1]の0番目の数字2を取り出せて、Just 2 が帰ってくる、ようにしたい）
indirectTwiceDup :: [Int] -> Int -> Maybe (Maybe Int)
indirectTwiceDup l x = (l !?) <$> (l !? x)

-- 残念ながら、このindirectTwiceの型は [Int] -> Int -> Maybe (Maybe Int) となってしまう

-- 一回目か二回目で失敗してたらNothing、両方成功ならJustを一個にする
flatten :: Maybe (Maybe a) -> Maybe a
flatten x =
  case x of
    Nothing -> Nothing
    Just y -> y
    -- これは、つまり
    -- Just (Just z) -> Just z
    -- Just Nothing  -> Nothing

-- これを使って、indirectTwiceを書き直す
indirectTwice :: [Int] -> Int -> Maybe Int
indirectTwice l x = flatten ((l !?) <$> (l !? x))

-- この mayflatmap は flatten . fmap と等価である
mayflatmap :: (a -> Maybe b) -> Maybe a -> Maybe b
mayflatmap f x =
  case x of
    Nothing -> Nothing
    Just y -> f y

-- これを使って、indirectTwiceを書き直す
indirectTwice2 :: [Int] -> Int -> Maybe Int
indirectTwice2 l x = (l !?) `mayflatmap` (l !? x)

{-
-- ろうとの形だと思ってほしい
(=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
(=<<) = mayflatmap

-- 先に計算するのは Maybe a のほうなのだから、逆向きのやつもほしい
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) = flip mayflatmap
-}

indirectThrice l x = (l !? x) >>= (l !?) >>= (l !?)
indirectQuince l x = (l !? x) >>= (l !?) >>= (l !?) >>= (l !?)

listflatmap :: (a -> [b]) -> [a] -> [b]
listflatmap f x =
  case x of
    [] -> []
    y : ys -> f y ++ listflatmap f ys

{-
元ネタ：ABC049C - 白昼夢 / Daydream
https://atcoder.jp/contests/abs/tasks/arc065_a

Tを空文字列の状態から始め、dream dreamer erase eraser のいずれかを追加する操作をn回行った文字列を全部列挙してみる
-}

-- 文字列に、dream dreamer erase eraser のいずれかを追加する操作を一回行った文字列を全部列挙する
dream1 :: String -> [String]
dream1 x =
  [
    x ++ "dream",
    x ++ "dreamer",
    x ++ "erase",
    x ++ "eraser"
  ]

dreamN :: Int -> String -> [String]
dreamN 0 x = [x]
dreamN n x = dreamN (n - 1) x >>= dream1

-- ここで、dreamN 2 "" は、 [] >>= dream1 >>= dream1 と等価である
-- >>> dreamN 2 ""
-- ["dreamdream","dreamdreamer","dreamerase","dreameraser","dreamerdream","dreamerdreamer","dreamererase","dreamereraser","erasedream","erasedreamer","eraseerase","eraseeraser","eraserdream","eraserdreamer","erasererase","erasereraser"]


-- 2.3 do記法でのびやかにMonadを使う

indirectPlus :: [Int] -> Int -> Maybe Int
indirectPlus l x =
  (l !? x) >>= (\y ->
    (l !? y) >>= (\z ->
      (l !? (y + z)) >>= (\w ->
        Just w
      )
    )
  )

-- do記法は何らかの Monad m に対して、型 m a の式を書くための糖衣構文です。
indirectPlus2 :: [Int] -> Int -> Maybe Int
indirectPlus2 l x = do
  y <- l !? x            -- l !? x >>= (\y ->
  z <- l !? y            -- l !? y >>= (\z ->
  w <- l !? (y + z)      -- l !? (y+z) >>= (\w ->
  pure w   -- doの最後の行には、m a の値を起きます

{-
do記法の各行は、次のように展開されます：
(var :: a) <- (mexp :: m a) 
  という行は、 mexp >>= (\var -> という形に展開され、それより下の行で var を使うことができます。
mexp :: m () 
  という行は、mexp >>= (\_ -> という形に展開されます。（結果は使いません）
let (var :: a) = (exp :: a)
  という行は、let var = exp in という形に展開されます。
-}


indirectPlusSquare :: [Int] -> Int -> Maybe Int
indirectPlusSquare l x = do
  y <- l !? x
  z <- l !? y
  let index = (y + z) ^ 2 -- doの内部では、let から始まる行でローカルな変数を定義できます。
  w <- l !? (y + z)
  pure (w ^ 2) -- doの最後の行には、m a の値を起きます

-- 糖衣構文を展開すると、次のようになります：
indirectPlusSquare2 :: [Int] -> Int -> Maybe Int
indirectPlusSquare2 l x =
  (l !? x) >>= (\y ->
    (l !? y) >>= (\z ->
      let index = (y + z) ^ 2 in
        (l !? (y + z)) >>= (\w ->
          Just (w ^ 2)
        )
    )
  )