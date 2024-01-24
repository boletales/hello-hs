# Haskell入門

## 第0章 はじめに
### 0.1 モチベーション
- このドキュメントでは、Haskellを実用できるようになることを目標とする
  - 具体的に、環境構築と基本的な文法から始めて、最終的にパーサコンビネータまで扱いたいと考えている
  
- みなさんは以下のようなスタイルのプログラミングをしたことがあるだろうか：
  - 入力や表示を行う部分と、ただ計算をするだけの部分を分離する
  - グローバル変数の使用を避ける
  - 引数だけに依存し、副作用を起こさず値を返す小さな関数ををたくさんつくる
  - 関数の引数としてラムダ式を渡したり、返り値としてラムダ式を返したりする
- ↑の1つ目と2つ目はプログラミング慣れした人々にとって一般的な慣習だと思う
- これに3つ目と4つ目を加えたのが、いわゆる「関数型プログラミング」というスタイルである
  
- あるいは、みなさんは次のような感覚や行動に心当たりがないだろうか：
  - 関数を書くとき、副作用が起こらないように「気をつけた」ことがある
  - 配列に対するforEachでは躊躇なく副作用を起こすが、mapの中で副作用を起こすのは筋が悪いように感じる
  - 式に型が付いていると安心するが、どこにでも型を書かされるのは煩わしいと思う
  - 開発者コンソールやREPLでワンライナーを書くのが好きである
  - メソッドチェーンを使うのは楽しいと思う

- プログラミングの文脈において、「関数」という言葉には次の二側面が含まれている：
  - 関数とは、「手続きの流れを切り出したもの」である。値を受け取ったり値を返したりすることもある
  - 関数とは、「値を受け取って、値を返すなにか」である。（Haskellにおいてはそうでないが、）その途中でなにか他のことをするかもしれない
- 多くの「ふつうのプログラミング言語」では、前者の意味での「関数」が言語機能として用意されている
  - 特に関数型っぽい書き方をする際には、前者を後者で模倣するために、前者を後者だとがんばって思い込むことを強いられる
- いわゆる「関数型プログラミング言語」と呼ばれる種類の言語では、「関数」はおおかた後者の意味のように振る舞う

- Haskellは、快適に関数型プログラミングをするためのプログラミング言語である
  - 関数が「値を受け取って値を返し、他にはないにもしないもの」としてふるまうことを言語仕様で実現している
- 具体的な特徴：
  - 関数は「ふつうの値」なので、引数として渡したり、返り値として返したりしてよい
  - すべての関数は純粋であり、いかなる場合でも関数適用はその値に置き換えても構わない（と約束する）
    - つまり、他の言語のように書くならば、`hoge(42) == true` ならばプログラム中の `hoge(42)` を全部 `true` に置換して構わないということである
    - コンパイラにとっては「返り値が変わらないならどんなアグレッシブな最適化も許容される」ことを意味する
  - すべての式には型が付いており、コンパイル時に検査される
    - ただし、型推論が強いので、型注釈をほとんど書かなくても正しく動く
  - コンパイラによる強烈な最適化が効きさえすれば、同じ実装課題についておおむねJavaと等速で（C比3倍程度）動作する
    - 「他の手続き型プログラミング言語で同じように関数型の書き方をしたコード」と比較すると異様に速い
  - 他の言語で副作用と呼ばれるものは、HaskellではIOアクションと呼ばれる種類の純粋な値である（この話はややこしいのでしばらく忘れていてほしい）
    - IOアクションは、javascriptで例えれば「勝手に発火せず入れ子にできる行儀のよいPromise」のようなものである
    - Haskellにおいて、プログラマのやることはエントリポイントとなるクソデカPromiseを組み立てることである

- 最後に具体的なコード例を示すことにする。ここで出てくるさまざまな要素はのちほどで説明する：
```haskell
main :: IO ()
main = putStrLn =<< (tanuki <$> getLine)

-- 文字列から「た」を抜く関数
tanuki :: String -> String
tanuki = filter (\c -> c /= 'た')

-- ↓HLSを導入しているエディタ上では、こういうコメントを書くと式の評価結果を確認できる
-- >>> tanuki "こたんたたにたちたは"
-- "こんにちは"
```
- しょっぱなから`->`だとか`<$>`だとか、あげくの果てに`=<<`だとかいう記号を見せられて面食らってるだろうが、あとで全部説明するから信じてほしい


### 0.2 環境構築
- ツールチェーンマネージャの [GHCup](https://www.haskell.org/ghcup/) を使うと手っ取り早い（2024年時点で推奨のインストール方法）
  - 参考：（これらのコマンドは公式のページからコピペしたほうが確実だと思う）
  - Windowsの場合、PowerShell上で `Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }`
  - Linuxの場合、 `{curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh}`
- 途中「stackを入れるか」「hlsを入れるか」と訊かれるが、それぞれyを選んでほしい
- その他の質問にはデフォルトの回答をしておけばよかったはず
- インストールされるもの
  - GHCup（Haskellのツールチェーンを管理するやつ）
  - GHC（コンパイラ）
  - cabal（パッケージマネージャ）
  - stack（cabalのラッパー）
  - hls（エディタの補完をしてくれるやつ）
- Windowsの場合は、ここまで終わったら一度再起動することを推奨する

- VSCodeに拡張機能 haskell.haskell をインストールする
- ここまでで、Haskellの環境構築は完了した（便利な時代になりました）

## 第1章 関数やリストとたわむれる
### 1.1 ghci（デバッグに便利な対話環境）の使い方
- ターミナルで `ghci` というコマンドを打つと対話環境が起動する
- `100 + 200` とか、`2 ** 10` とか、適当に数式を打ってみよう！評価してくれるぞ！
- `someconst = 3` みたいに定数を定義すると、あとで使うことができる
  - 変数の名前はすべて小文字で始める必要がある
  - 大文字で始まる名前は、具体的な型の名前として認識される（後で出てくる）
- 関数はふつうの値なので、`nibai = \x -> x * 2` みたいに打てば定義できる
  - ここで、`\x -> x * 2` は「引数xを受け取ってx * 2を返す関数」という意味
  - 関数を定義したかったら `sanbai x = x * 3` のように書くこともできる
- 関数を使うには、関数のあとにスペースを空けて引数を書けばいい
  - `nibai 3` と打つと、`6`が帰ってくる
- ghciは`:q` で終了できる

- どこか適当なフォルダに、`hoge.hs` というファイルを作って、以下のように書いてみる
  - （この先しばらく、VSCodeで`hoge.hs`とターミナルを開きながら作業する）
```haskell
-- ↓コメント
-- Haskellのプログラムは基本的に、定数の宣言でできている
someconst = 3

nibai = \x -> x * 2

sanbai x = x * 3
```
- ghciを起動して、`:l hoge.hs` と打つと、`hoge.hs` の宣言が読み込まれる
- 当然 `someconst` と打てば `3` が返ってくるし、`nibai someconst` と打てば `6` が返ってくる

#### 節末問題
- ここまでの知識だけで、「二個の引数を取る関数」は作れるだろうか？
  - たとえば、`(kakezan 3) 4` と打つと、`12` が返ってくるような関数を組み立てることはできるだろうか？
  - ヒント：Haskellにおいて関数はふつうの値の一種だから、関数の結果として返してもよい

### 1.2 関数と型
- 節末問題の答えはこんなふうである：
```haskell
-- ↓これが複数行コメント
{-
kakezan をこんなふうに定義すると、 kakezan 3 は \y -> 3 * y という関数になる
そうしたら、(kakezan 3) 4 は 3 * 4 = 12 になる
-}
kakezan = \x -> (\y -> x * y)
```
- ところで、いちいち `(kakezan 3) 4` とするのも、 `\x -> (\y -> x * y)` と打つのもまどろっこしいから、次のように書けるようになっている
```haskell
-- これらはいずれも上の kakezan と同じ意味である
kakezan2 = \x y -> x * y 
kakezan3 x y = x * y 

-- 関数適用は左結合で、優先度は最大である
_3x4 = kakezan 3 4
```
- Haskellにおいて「多変数関数」のように見えるものは、「引数を一個取って、のこりの引数を受け取る関数を返す関数」である

- また、関数の引数として関数を渡してもかまわない
```haskell
-- 二引数関数・整数・整数を受け取って、計算して返す
keisan f x y = f x y

-- 中置演算子は丸括弧で囲むとふつうの関数のように扱える
_4x5 = (*) 4 5

tashizan = keisan (+)
hikizan  = keisan (-)

_9plus8 = tashizan 9 8 -- 17
```
- ところで、このような無秩序なラムダ式の組み合わせは、型が付いていなければ厄災そのものである（過激な思想）
- Haskellでは、トップレベルの宣言に「型シグネチャ」というものを付けて型を明示することができる
```haskell
-- 二引数関数・整数・整数を受け取って、計算して返す
keisan :: (Int -> Int -> Int) -> Int -> Int -> Int
keisan f x y = f x y

-- 中置演算子は丸括弧で囲むとふつうの関数のように扱える
_4x5 :: Int
_4x5 = (*) 4 5

tashizan :: Int -> Int -> Int
tashizan = keisan (+)

hikizan  :: Int -> Int -> Int
hikizan  = keisan (-)

_9plus8 :: Int
_9plus8 = tashizan 9 8 -- 17
```
- `a -> b` は「aを受け取ってbを返す関数」の型を表す
- `->` は右結合である。つまり、 `a -> b -> c` は `a -> (b -> c)` と同じである
  - これは、「`a`を渡すと『`b`を受け取って`c`を返す関数』を返す関数」である
  - つまり、「`a`と`b`を渡すと`c`を返す関数」である
- 型シグネチャはつけなくてもおおかたの場合正しく動くが、エラーメッセージがめちゃくちゃわかりづらくなるので付けるべきである

- 型のわからない式は、ghciで `:t (式)` と打つと型を表示してくれる
  - 試しに、`:t keisan` と打てば `(Int -> Int -> Int) -> Int -> Int -> Int` と表示されるはずである
  - それじゃあ、`:t 4` と打つと……？
    - `4 :: Num a => a` というなにやら新しい記法が出てくるが、これの話は1.5節あたりで「型クラス」という言葉を説明する際にする

- 具体的な型の付かなさそうな式についても `:t` をつかってみよう
  - `:t (\x y -> y x)` と打つと……？
    - `(\x y -> y x) :: t1 -> (t1 -> t2) -> t2` と表示される
    - ここで、小文字で始まる型（`t1`とか`t2`）は、そこにどんな型が入ってもよい「型変数」というものである
    - さっきの `Num a => a` のうち、`Num a` は `a` が満たす条件、そのあとの `a` は式の型である（が、詳細な説明はあとまわしにする）

#### 節末問題
- 実は、`keisan` は次のように定義しても同じような意味になる。なぜだろう？
  - `keisan4` の型と、`keisan4` に3つ引数を渡したときの結果が一致することを確認してほしい
```haskell 
keisan4 f = f
```
- 「二引数関数」が与えられたときに、その引数の順番を入れかえるような関数は作れるだろうか
  - 具体的に、`myflip` という関数を作って、`myflip hikizan 3 10` が `7` になるようにしたい

### 1.3 パターンマッチ・再帰
- そろそろ、条件分岐がないとおもしろいサンプルが書けないことに気づいて退屈してきたころだろうか
- Haskellにおいて、条件分岐はパターンマッチで実現される。（糖衣構文としてIf式も存在する）
```haskell
collatz :: Int -> Int
collatz x =
  case x `mod` 2 of
    0 -> x `div` 2
    _ -> x * 3 + 1 -- ここで _ は「どんな値でもよい」という意味
```
- case式の結果は、`case` と `of` の間の式が始めに一致したパターンの`->`以降の式を評価したものである

- 関数の定義にそれ自身が出てきた場合には再帰呼び出しとして扱われる
  - ML系と違って `let rec` を書く必要はないが、typoで意図せず再帰になって無限ループすることもある（<20敗）ので注意
- 実は、Haskellにおいては評価戦略の都合上、末尾再帰にしなくてもよい
  - なんなら末尾再帰にしない方が最適化が効きやすくて好ましいらしい
```haskell
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
```

- case式のどのパターンにも当てはまらなかった場合はエラーになって終了する
  - これは「部分関数」と呼ばれる類いのものだが、部分関数はろくな代物でない（過激な思想）ので書くべきでない
  - Haskellの標準ライブラリにはなぜか部分関数が多数含まれている。ﾅﾝﾃﾞﾀﾞﾛｳﾈｰ（怒）

#### 節末問題
- 正整数の非負整数乗を計算する関数 `power :: Int -> Int -> Int` を、再帰で定義してみてほしい

### 1.4 自家製の型・できあいの型・型変数
- Haskellのよい特徴として、自家製の型をかんたんに作れることが挙げられる
  - この特徴は、遠戚のML系言語にもあり、これらの影響を受けて他の言語にも輸入されている。すばらしい
- 型は、次のような構文を用いて定義できる
```haskell
-- 整数のリスト
data IntList =
    ILNil
  | ILCons Int IntList
  deriving Show -- ← ghciで値を表示できるようにするおまじない（後で説明する）
```
- はじめの `data IntList` は、`IntList` という名の新しい型を定義するという意味である
- その後の部分は、`IntList` の値を作る方法の定義である
  - `ILNil` や `ILCons` は、(値)コンストラクタという関数のようなものである
    - ただし、関数と違って、これらを使ってパターンマッチすることができる
    - その他の特徴は関数と同じである
  - `ILNil :: IntList` という値コンストラクタは、 `IntList` の値の一つである
  - `ILCons :: Int -> IntList -> IntList` は、`Int` と `IntList` を受け取って `IntList` を返す値コンストラクタである
    - 具体的に、`ILNil` とか `ILCons 3 ILNil` とか、あるいは `ILCons 3 (ILCons 4 ILNil)` とかは `IntList` の値である

- ↑で定義した `IntList` を使って関数を定義してみよう
```haskell
ilSum :: IntList -> Int
ilSum x =
  case x of
    ILNil -> 0
    ILCons y ys -> y + ilSum 
-- ↑ パターンマッチの際には、値コンストラクタを用いてパターンを書くことができる
--   また、値コンストラクタの引数として、変数を書くことも、具体的な値を書くこともできる
```

- 型を定義する際、それに型変数を持たせることもできる
```haskell
data List a =
    LNil
  | LCons a (List a)
  deriving Show -- ← ghciで値を表示できるようにするおまじない（後で説明する）
```
- ここで、`List` は型引数を一つ受け取ると具体的な型になる「型コンストラクタ」である
- `List` に `Int` を渡した `List Int` は具体的な型で、これは先ほどの `IntList` と同じように使える
```haskell
lSum :: List Int -> Int
lSum x =
  case x of
    LNil -> 0
    LCons y ys -> y + lSum ys
```
- また、`List` にどんな型が入っていても同じように使える関数も定義できる
```haskell
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
```

- 「計算に失敗して値が返ってこないことがある」ということを表す、RustのOption型を移植してみよう
```haskell
data Option a =
    None
  | Some a
  deriving Show -- ← ghciで値を表示できるようにするおまじない（後で説明する）

-- リストの先頭を返す（失敗することもある）
lHead :: List a -> Option a
lHead x =
  case x of
    LNil -> None
    LCons y ys -> Some y
```

- Haskellには、このように定義された型がすでにいくつか用意されている
  - `Bool` は `data = False | True` というように定義されている
  - RustのOption型は、Haskellでは `Maybe` と呼ばれている
    - `Maybe a` は、先ほどの `Option a` と同じようなものである
    - `Maybe` は、`Nothing` と `Just a` という値コンストラクタを持つ
  - `[a]` は `a` のリストの型で、その値は `[] :: [a]` と `(:) :: a -> [a] -> [a]` で作られる
    - `(:)` は右結合の中置演算子である
    - 具体的に、`1 : (2 : '3 : []')` のような値を作れる
    - リストの値を書くための糖衣構文として、`[1, 2, 3]` と書くことができるが、これは↑と同じ値である
    - これは、基本的に先ほどの `List a` と同じものである


#### 節末問題
- `IntList` や `List a` どうしを結合する関数、`ilappend :: IntList -> IntList -> IntList` や `lappend :: List a -> List a -> List a` を定義してみよう

### 1.5 型クラス
- ghci上で `:t 42` としたときに表示された `Num a => a` とは、あるいは、`(+) :: Num a => a -> a -> a` とは、どういう意味だろうか？
  - 型シグネチャに `=>` が出てきた場合、左側は「型クラス制約」と呼ばれる型に関する条件をあらわすもので、右側が型本体である
  - ここで出てきた `Num` は、「型クラス」と呼ばれるものの一つで、述語論理における「述語」のようなものである
  - `Num` の具体的な意味は「和・差・積・絶対値・符号の取り出し・整数からのキャスト ができる」である
  - 制約 `Num a` を満たす具体的な型として、`Int` や `Double` がある

- 型クラスは、Javaのinterfaceに類似した概念（型クラスのほうが表現力が高い）であり、Rustにおけるtraitの元ネタである

- 型クラスを定義する際、その型クラスを満たす型に対して必ず実装すべき関数を定義することができる
  - 具体的に、ある型 a について `Num a` が成り立つ場合は、必ず以下の関数が定義されている
    - `(+) :: a -> a -> a`
    - `(-) :: a -> a -> a`
    - `(*) :: a -> a -> a`
    - `signum :: a -> a`
    - `abs :: a -> a`
    - `fromInteger :: Integer -> a` （Integerは多倍長整数の型）
  - 自分で作った型 `SomeType` について `Num SomeType` が成り立つようにしたかったら、これらの関数を定義する必要がある

- 型クラスを具体的に定義して、ここまで挙げたいろいろな型がそれを満たすようにしてみよう
```haskell
-- まずは、型クラスを定義する
class MyMonoid a where
  mymempty :: a -- モノイドには単位元がある
  mymappend :: a -> a -> a -- モノイドには結合則がある

-- 次に、ここまで出てきた型が MyMonoid を満たすようにする
instance MyMonoid IntList where
  mymempty = ILNil
  mymappend x y = -- 節末問題を解いていたら、 myappend = ilappend としてよい
    case x of
      ILNil -> y
      ILCons z zs -> ILCons z (mymappend zs y)

instance MyMonoid (List a) where
  mymempty = LNil
  mymappend x y = -- 節末問題を解いていたら、 myappend = lappend としてよい
    case x of
      LNil -> y
      LCons z zs -> LCons z (mymappend zs y)

instance MyMonoid [a] where
  mymempty = []
  mymappend x y = x ++ y -- ++ はリストの結合演算子
```
- こうして型クラスを作ってしまえば、これを満たす型についてそれぞれ動作する関数を書くことができる
```haskell
double :: MyMonoid a => a -> a
double x = x `mymappend` x -- `` で囲むと中置演算子として扱える

-- MyMonoid を満たす [Int] についても IntList についても、おなじように double を使える
doubledList    = double [1, 2, 3] -- [1, 2, 3, 1, 2, 3]
doubledIntList = double (ILCons 1 (ILCons 2 ILNil)) -- ILCons 1 (ILCons 2 (ILCons 1 (ILCons 2 ILNil)))
```

- 型クラスは複数の引数を取ることもできる
```haskell
class Listish elem listish where
  tolist :: listish -> [elem]
  fromlist :: [elem] -> listish

instance Listish a [a] where
  tolist x = x
  fromlist x = x

instance Listish a (List a) where
  tolist x =
    case x of
      LNil -> []
      LCons y ys -> y : tolist ys
  fromlist x =
    case x of
      [] -> LNil
      y : ys -> LCons y (fromlist ys)

instance Listish Int IntList where
  tolist x =
    case x of
      ILNil -> []
      ILCons y ys -> y : tolist ys
  fromlist x =
    case x of
      [] -> ILNil
      y : ys -> ILCons y (fromlist ys)

-- 使ってみる
listishHead :: Listish elem listish => listish -> Maybe elem
listishHead x =
  case tolist x of
    [] -> Nothing
    y : _ -> Just y

-- 型クラス制約を複数書く場合は、次のようにする
listishSum :: (Listish elem listish, Num elem) => listish -> elem
listishSum x =
  case tolist x of
    [] -> 0
    y : ys -> y + listishSum (fromlist ys)
```

- 型クラスのインスタンスを書く際、型クラス制約を用いてもよい
```haskell
class Maybeish elem maybeish where
  tomaybe :: maybeish -> Maybe elem
  frommaybe :: Maybe elem -> maybeish

instance Listish elem listish => Maybeish elem listish where
  tomaybe x =
    case tolist x of
      [] -> Nothing
      y : _ -> Just y
  frommaybe x =
    case x of
      Nothing -> fromlist []
      Just y -> fromlist [y]
```

- 型クラスを用いた多相（関数を複数の型に対応させること）はHaskellの書き味の根幹といって差し支えない
- Haskellの標準ライブラリの関数の多くは、型クラス制約を用いることで複数の型に対応している
- 標準ライブラリで定義された型クラスのうち、頻繁に使うものには次のような例がある： 
  - `Eq`      : 等価比較ができる
  - `Ord`     : 順序比較ができる
  - `Show`    : `String`に変換できる
  - `Read`    : `String`から変換できる
  - `Num`     : 和・差・積・絶対値・符号の取り出し・整数からのキャスト ができる
  - `Monoid`  : モノイドである
  - `Functor` : 配列に対するmapと同じようなことができる（第2章のメインテーマである）

- `Eq`・`Show` にはデフォルト実装が用意されているため、自分で型を定義したあと `deriving (Show, Eq)` と書けば `instance` で定義しなくてもよい
  - 独自の実装をしたい場合には instance を書けばよい

- 実は、Haskellの `String` 型は `[Char]` のエイリアスである
- 独自にShowを実装する例：
```haskell
data Stars = Stars Int
instance Show Stars where
  show (Stars n) = 
    case n of
      0 -> "" -- ['']と同義
      _ -> '*' : show (Stars (n - 1))
```

- ここまでで型の記法を理解したので、型を入力するとそれに対応する関数を検索できるマジで最高のサイト [Hoogle](https://hoogle.haskell.org/) を紹介する
- さっき作った `lappend` のような結合をする関数を探してみよう
  - 検索窓に `[a] -> [a]  -> [a]` と打つと、`(++)` という関数がヒットする
- 配列に対するmapと同様の働きをする関数も探してみよう
  - 検索窓に `(a -> b) -> [a] -> [b]` と打つと、`map` という関数がヒットする
- これらの検索結果をクリックすると、対応するモジュールのドキュメントを閲覧できる

#### 節末問題
- `(\f x y -> f y x)` という関数は、標準ライブラリでは何と呼ばれているだろうか？
  - ヒント；これの型を考えて、Hoogleで検索し、ドキュメントを読んでほんとうにそう振る舞うか確認しよう

- 型 `V1D`・`V2D`・`V3D` を次のように定義した。ノルム空間を表す型クラス `RNorm` を定義し、これらの型が `RNorm` を満たすようにしてみよう
```haskell
data V1D = V1D Double
data V2D = V2D Double Double
data V3D = V3D Double Double Double
```
