# Haskell入門

## 第0章 はじめに
### 0.1 モチベーション
このドキュメントでは、Haskellの環境構築と基本的な文法を学んだのち、パーサコンビネータライブラリを用いた程度実用的なプログラムを書くことを目標とします。読者として、いままでに2つ以上のプログラミング言語を学んだことのある人を想定しています。

Haskellは、「静的型付き純粋関数型プログラミング言語」と呼ばれることがあります。この言葉は、静的に型の付く「純粋な関数」を用いて「関数型プログラミング」と呼ばれるスタイルのプログラミングをすることに特化した言語であるということを意味しています。それでは、「純粋な関数」とか「関数型プログラミング」とは何で、どのような恩恵をもたらしてくれるのでしょうか。

プログラミングの文脈において、「関数」という言葉には「いろんな命令を実行して最後に値を返す手続き」としての側面と、「値を変換する写像」としての側面があります。
```C
int someGlobalVar = 0;

// 手続きの流れを切り出したもの（値が返ってくることもある）
int procedure(int x) {
  someGlobalVar += x;
  printf("%d\n", someGlobalVar);
  return someGlobalVar;
}

// 値を受け取って値を返すもの（手続きをすることもある）
int converter(int x) {
  return x * 2;
}
```
関数型プログラミングという言葉に明確な定義はありませんが、端的に言えば関数の「写像」としての側面を多用するスタイルのプログラミングのことを指します。プログラムの仕様を説明する際に「○○を××したもの」という多く出てくるようであれば、関数型プログラミングの恩恵を強く受けられます。例として、Pythonで書かれた次のコード片を見てみましょう。
```python
# 配列の各要素を二乗して、30より小さいものだけ取り出すコード

original = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# 手続き型のアプローチ
squared = []
for x in original:
  squared.append(x ** 2)

filtered = []
for x in squared:
  if x < 30:
    filtered.append(x)

# 関数型のアプローチ
result = filter(lambda x: x < 30, map(lambda x: x ** 2, original))
```
手続き型のアプローチでは、計算機の振る舞いや計算量は想像しやすいですが、一方でコード全体の流れは元の説明を「翻訳」したようなものになっています。これと比較すると、関数型のアプローチでは、コード全体の流れが「○○を××したもの」という説明に近くなっています。現代的なプログラミング言語を使っている限り、おそらく私たちは「関数型プログラミング」の香りなしにすべてのコードを書くことはないはずです（PythonやJSを主に使っていて、この先mapやfilterを封じられても全く構わない、という奇特な方がいたらプルリクで教えてください）。

しかしながら、「ふつうのプログラミング言語」において、関数という言語機能は「写像」としてではなく、「いろんな命令を実行して最後に値を返す手続き」として用意されています。このような言語で関数型プログラミングをしようとする限り、私たちは「値を返す手続き」を「写像」だとがんばって思い込むことを強いられますし、「値を返す手続き」を手なずけて「写像」のように振る舞わせるために、いろんなことに気を遣わなければいけません（学部の講義でPythonを使ったことがあるならば、引数として配列を渡したあとうっかりオリジナルを書き換えてバグらせたことがあると思います）。

関数型プログラミング言語を習得していない人は、仕様の中に「△△は○○を××したものである」という文が増えれば増えるほど、翻訳調の手続き型プログラミングと快適でない関数型プログラミングの間で選択を迫られることになります。

これは無用な苦痛です。関数型プログラミングが上手く填まる場面にはHaskellのような関数型プログラミング言語を持ってくるべきなのです。
```haskell
-- これがHaskellのコードです

filterOddThenSquare :: [Int] -> [Int]
filterOddThenSquare list = filter (\x -> x `mod` 2 == 1) ((^ 2) <$> list)
```
「関数型プログラミング言語」という言葉にも明確な定義はありませんが、おおむね「関数型プログラミングが快適」「関数を見て『写像』だと納得しやすい」という特徴を言語仕様で実現しているプログラミング言語がこう呼ばれます。これに加えて、Haskellには関数型プログラミングをさらに快適にするためにいくつか珍しい特徴を持ちます。

Haskellの特徴について軽く紹介します：

1. 関数は「ふつうの値」であり、変数に入れたり、引数として渡したり、返り値として返したりできる
   
    これは、現代的なプログラミング言語の間では広く普及した特徴かもしれません。関数型プログラミングを快適にするためには必須です。

2. すべての関数は「純粋」であり、いかなる場合でも関数適用はその値に置き換えても構わない

    これはHaskellをはじめとした「純粋関数型プログラミング言語」だけの特徴です。「関数が純粋」は他の言語で例えるならば、「`hoge(42) == 100` が成立するならいつだって`hoge(42)`の値は`100`なのだから、プログラム中の `hoge(42)` を全部 `100` に置換しても全く問題ない」という意味です。Haskellではすべての関数についてこれが成り立っているので、コンパイラはかなりアグレッシブに定数畳み込みのような最適化を行うことが許されます。

3. すべての式にはコンパイル時点で型が付いていて、安全なNullやエラーのための型がある

    冒頭に一瞬出てきた「静的型付き」の部分のことです。Haskellの文化では、TSとかRustのようにとにかくカジュアルに型を定義して、いろいろなことを型で表現しようとします。代数的データ型とパターンマッチは早くすべてのプログラミング言語に輸入されるべきです。

4. 他の言語でいう副作用は「IOアクション」とよばれる種類の値として表現される
  
      「IOアクション」というのは、「値を返す手続きの『やり方』」のことで、JavaScriptで例えれば「勝手に発火しないし入れ子にもできるお行儀のよいPromise」のようなものです。Haskellにおいて、プログラマの仕事は、エントリポイントとなる一つの大きなIOアクションを組み立てることです。（JSを触ったことがある皆さんはご存じの通り、）手続きを組み立てることとそれが実行されることを分離して考えるのは、非同期やマルチスレッドの要素を含むWebサーバーのプログラムを書く上でとても有益です。

5. コンパイラによる強烈な最適化が効きさえすれば、同じ課題についてJavaぐらい（Cの1/3ぐらい）の速度で動作する

    「同じ課題について」であって、「同じように関数型の書き方をしたコードについて」ではないことに注目してください。Haskellで書いたプログラムは、関数型プログラミングに最適化されていない言語で同じ関数型の書き方をしたプログラムよりはるかに高速に動作します。

6. 「関数を組み合わせるための関数」がめちゃくちゃ豊富に用意されている

    「ふつうのプログラミング言語」で関数型プログラミングをする際の不愉快な点の一つは、小さい関数をたくさん書くとそれを組み合わせた式がまどろっこしい見た目になることです。Haskellには大量の中置演算子（mapとか、関数合成とか、モノイドの結合とか）が用意されており、ユーザーが新しく定義することもできます。

7. パフォーマンスが予測しづらく（たいていJSよりは速い）、メモリ管理はGCに一任されている

      コンパイラがマジカルな最適化を多数行うため、パフォーマンスを改善したければプロファイリングが必須です。また、残念ながら、値の不変性やモナドのような激強構造はかっちりとしたメモリ管理とは相性がよくないようです。厳密なメモリ管理やガチガチのパフォーマンスチューニングを求められるような用途であれば、おとなしくRustを使ってください。

これらの特徴がうまくかみ合うような場面でHaskellを使えば高い生産性を実現できます。コンパイラや自作言語の実装においてHaskellの右に出る言語はおそらくないですし、Webサーバーのような副作用の流れが複雑な場面でも優秀です。上の特徴にがっつりかみ合う場面でなくても、普段使い用のまともに型が付いてスクリプトっぽい書き味が実現できる言語、つまるところPythonしか知らない人のPython代替としてはそれなりの使いやすさがあります。（実際に、GHCのインタプリタモードを使えばスクリプトとして実行できます）

逆に、これらの特徴が最悪の方向にかみ合うような場面、つまるところコードと機械語の対応が明確だったり、メモリ管理の厳密さが求められるような場面には向いていないので、そういうときにはおとなしくRustを使ってください。

最後に、「Haskellには副作用がないからHello Worldも大変そう」という懸念を払拭するために、IOを使った短いコードを置いておきます。

```haskell
-- 一行読んで、たを抜いて、表示する
main :: IO ()
main = (tanuki <$> getLine) >>= putStrLn

-- ふつうの言語っぽい見た目でも書ける
-- 注意：doとかいうやつのことは、第2章で説明するまできれいさっぱり忘れておくこと！
main2 :: IO ()
main2 = do
  line <- getLine
  putStrLn (tanuki line)

-- 文字列から「た」を抜く関数
tanuki :: String -> String
tanuki = filter (\c -> c /= 'た')

-- ↓Haskell Language Serverとエディタのプラグインを導入すれば、こういうコメントを書いてエディタ上で式の評価結果を確認できる
-- >>> tanuki "こたんたたにたちたは"
-- "こんにちは"
```


### 0.2 環境構築
2024年現在は、ツールチェーンマネージャの [GHCup](https://www.haskell.org/ghcup/) を使うことが推奨されています。過去にはインストール方法が混迷を極めていた時代もありましたが、現時点ではWindowsでもMacでもLinuxでもGHCupを使えば一発で環境構築ができます。インストールの途中で「Stackをインストールするか？」「HLS(Haskell Language Server)をインストールするか？」と訊かれますが、これにはどちらもyと答えてください。Windowsを使っている場合は、インストール後に環境変数の変更を反映させるため、一度再起動してください。

VSCodeに拡張機能「Haskell」（id: haskell.haskell）をインストールすれば、環境構築は完了です。

## 第1章 関数やリストとたわむれる
### 1.1 ghci（デバッグに便利な対話環境）の使い方
GHC（Haskellのコンパイラです）をインストールすると、ghciという対話環境が一緒についてきます。Haskellでプログラミングをする際には、ソースコードをghciに読み込ませて、関数の動作を確認しながら進めるのが一般的です。

ターミナルで `ghci` というコマンドを打ち、起動してみましょう。
```
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
ghci>
```

ここに式を入力すると、評価結果が表示されます

```
ghci> 100 + 200
300
ghci> sqrt 10                -- 関数適用は、関数 [空白] 引数 
3.1622776601683795
ghci> (\x -> x * x) 5        -- ラムダ式（その場で定義された関数）はこういう風に書ける
25
```

ghciの使用中に変数を定義することもできます（同名の定義を行うと、先に定義したほうは隠蔽される）。変数の名前はかならず小文字で始まる必要があります。

```
ghci> someconst = 3
ghci> someconst * 10
30
ghci> nibai = \x -> x * 2    -- 関数はふつうの値なので、当然変数につっこむことができる    
ghci> nibai 100
200
```

ghciを終了するには、`:q` と打ちます


```
ghci> :q 
Leaving GHCi.
```

ここまでの内容を `nyumon.hs` というファイルに書き込んで、ghciに読み込ませてみましょう。

```haskell
-- nyumon.hs

-- ↓行コメント
-- Haskellのプログラムは基本的に、定数の宣言でできている
someconst = 3

nibai = \x -> x * 2
```
再度ghciを起動して、`:l nyumon.hs` と打つと、`nyumon.hs` で行った宣言が読み込まれて使えるようになります。たとえば、`nibai 100` と打つと `200` と表示されるはずです。

ghciを開いたまま、`nyumon.hs` に次の内容を書き足してみましょう。
```haskell
-- 関数はこのように定義することもできる
sanbai x = x * 3
```

ghci上で`:r`と打つと、`nyumon.hs` の内容が再度読み込まれます。

このドキュメントでは、この先しばらく（第2章が終わるぐらいまで）は、一枚のファイルとそれを読み込んだghciで進めていくことにします。ファイルを編集したら、忘れずに `:r` するようにしてください（>20敗）。


#### 節末問題
1．ここまでの知識だけで、「二個の引数を取る関数」を作ることはできるでしょうか？
  - たとえば、`(kakezan 3) 4` と打つと、`12` が返ってくるような関数`kakezan`を作ってみてください
  - ヒント：関数はふつうの値の一種なので、関数を関数の結果として返してもよい

### 1.2 関数と型
節末問題の直後にネタバレを置くのもずいぶん申し訳ない話ですが、「二個の引数を取る関数」はこんなふうに作ることができます。
```haskell
-- ↓これが複数行コメント
{-
kakezan をこんなふうに定義すると、 kakezan 3 は \y -> 3 * y という関数になる
そうしたら、(kakezan 3) 4 は 3 * 4 = 12 になる
-}
kakezan = \x -> (\y -> x * y)
```
ところで、いちいち `(kakezan 3) 4` とするのも、 `\x -> (\y -> x * y)` と打つのもまどろっこしいですから、次のような略記が用意されています。
```haskell
-- これらはいずれも上の kakezan と同じ意味である
kakezan2 = \x y -> x * y 
kakezan3 x y = x * y 

-- 関数適用は左結合で、優先度は最大である
_3x4 = kakezan 3 4
```
Haskellにおいて「多変数関数」のように見えるものは、「引数を一個取って、のこりの引数を受け取る関数を返す関数」です。

また、関数を関数の引数として渡すこともできます。
```haskell
-- 二引数関数・整数・整数を受け取って、計算して返す
keisan f x y = f x y

-- 中置演算子は丸括弧で囲むとふつうの関数のように扱える
_4x5 = (*) 4 5

tashizan = keisan (+)

_9plus8 = tashizan 9 8 -- 17
```
トップレベルの宣言には「型シグネチャ」というものを付けて型を明示することができます
```haskell
-- ○○ -> ×× というのは、○○を受け取って××を返す関数の型を表す
square :: Int -> Int
square x = x * x

-- -> は右結合なので、こう書くと Int -> (Int -> Int) と同じ意味である
hikizan  :: Int -> Int -> Int
hikizan  = keisan (-)

_5plus8 :: Int
_5plus8 = tashizan 5 (8 :: Int) -- 式に型注釈を付けることもできる
```
Haskellの性質上、型を明示されていないところで型エラーを出すと、推論の結果として読みづらいエラーが出ることがあります。そのため、基本的にトップレベル宣言には型シグネチャを付けるようにしましょう。

式の型がわからない場合には、ghciで `:t (式)` と打つと型を表示してくれます。
```
ghci> :t hikizan               -- さっきの hikizan の型が表示される
hikizan :: Int -> Int -> Int
```

具体的な型の付かなさそうな式も見てみましょう。
```
ghci> :t (\x f -> f x)                                                   
(\x f -> f x) :: t1 -> (t1 -> t2) -> t2
```
型の表記の中で、大文字で始まる型は具体的な型、小文字で始まる型は「型変数」と呼ばれる、そこに何が入ってもよい部分を表すものです。

#### 節末問題
1. `keisan` は次のように定義しても同じような意味になることを確認してみましょう。
  - より具体的には、`keisan4` の型と、`keisan4` に3つ引数を渡したときの結果が一致することを確認してみてください
```haskell 
keisan4 f = f
```
2. 「二引数関数」が与えられたときに、その引数の順番を入れかえるような関数を作ってみましょう。
  - 具体的に、`myflip` という関数を作って、`myflip hikizan 3 10` が `7` になるようしてください

### 1.3 パターンマッチ・再帰
Haskellで条件分岐をする場合には、`case`式とパターンマッチを使います。
```haskell
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
```

関数の定義にそれ自身が出てきた場合には、再帰的に呼び出しが行われます。ML系と違い、再帰関数を定義する際も `let rec` のような特別な構文を使う必要はありませんが、typoして意図せず無限ループを引き起こしてしまうこともあるため注意が必要です（n敗）。Haskellで再帰関数を書く際には、評価戦略の都合上、末尾再帰にしなくてもパフォーマンス上のデメリットはありません。
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

-- 無限ループ！試さないこと
infinityLoop :: Int -> Int
infinityLoop x = infinityLoop x
```

case式のどのパターンにも当てはまらなかった場合、関数が例外を吐いて落ちます。これは「部分関数」と呼ばれ、ろくな代物ではないため自分で書くことは避けてください。書こうとしている関数の定義域が型の値全体にならないような場合には、後述の「Maybe」を使うのがHaskellらしいよい文化です。
```haskell
-- 部分関数
partialTest :: Int -> Int
partialTest x =
  case x of
    0 -> 0

-- >>> partialTest 5
-- [略]\nyuumon.hs:(101,3)-(102,10): Non-exhaustive patterns in case
```
ところで、Haskellの標準ライブラリ`Prelude`には、いったいどういうわけか部分関数が数多く含まれています。ﾅﾝﾃﾞﾀﾞﾛｳﾈｰ（怒）

#### 節末問題
1. 整数の正整数乗を計算する関数 `power :: Int -> Int -> Int` を、再帰で定義してみましょう。
   - Maybeの使い方は次節で説明するので、第二引数が0以下の場合に死ぬような部分関数にして構いません。

### 1.4 自家製の型・できあいの型・型変数・カインド
0.1節で述べたとおり、Haskellのよい特徴のひとつは様々な用途の型をカジュアルに定義できることです。Haskellや、その遠い親戚にあたるML系言語、その影響を受けたRustなどの諸言語には「代数的データ型」と呼ばれる素晴らしい言語機能が存在します。これは、型を「『他の型の直積』の名前付きの直和」として定義できるものです。
```haskell
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
```
ここで出てきた `MyTrue` や `MyFalse`、`BoolAndInt` や `IntAndInt` は「値コンストラクタ」と呼ばれるものです。これらを関数のように使って、その型の値を作ることができます。
```haskell
-- BoolAndInt (:: Bool -> Int -> BoolAndInt_Or_IntAndIntAndInt) や
-- IntAndInt  (:: Int -> Int -> Int -> BoolAndInt_Or_IntAndIntAndInt) は、
-- BoolAndInt_Or_IntAndIntAndInt の値コンストラクタであり、関数のように使える

bandi :: BoolAndInt_Or_IntAndIntAndInt
bandi = BoolAndInt MyTrue 3
```
また、値コンストラクタはパターンマッチに使うことができます。
```haskell
reverseOrAdd :: BoolAndInt_Or_IntAndIntAndInt -> Int
reverseOrAdd x =
  case x of
    BoolAndInt MyTrue i  -> -i
    BoolAndInt MyFalse i ->  i
    IntAndInt i j k      -> i + j + k

-- >>> reverseOrAdd bandi
-- -3
```
data宣言をする際、その型自身を値コンストラクタの引数にすることもできます。この場合、その型の値は帰納的に定義されることになります（残念ながらHaskellには帰納を帰納として扱う手段がないため、これらの型について明示的に帰納法を書くことはできず、再帰を使うことになります）
```haskell
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
```

data宣言の左辺には、型変数を書くことができます。この「型変数」というのは、そこに何か型を入れるための何をいれてもよいスペースのことです。
```haskell
-- 型 a の値たちのリスト
data List a =
    LNil
  | LCons a (List a)
  deriving Show

-- List Int や、 List MyBool は具体的な型

lIntTest :: List Int
lIntTest = LCons 1 (LCons 2 (LCons 3 LNil))

lBoolTest :: List MyBool
lBoolTest = LCons MyTrue (LCons MyFalse LNil)
```
`List` は型引数を一つ受け取ると具体的な型になる「型コンストラクタ」と呼ばれるものです。

「型である（`Int`や`IntList`）」「型変数を一つ受け取ると具体的な型になる（`List`）」などの、「型の型」のような属性のことを、Kindと呼びます。ghciで `:k List` と打つと、Kindを調べることができます。
```
ghci> :r
Ok, one module loaded.
ghci> :k Int
Int :: *
ghci> :k List
List :: * -> *
ghci> :k (List Int)
(List Int) :: *
```
`*` は、「具体的な型」を表すKindです。`List` は、具体的な型を一つ受け取って具体的な型になるので、Kindは `* -> *` となります。

型変数のKindはかならずしも`*`ではありません。
```haskell
-- newtype は、型コンストラクタが1種類のとき専用のdata宣言のようなもの（dataでもよい）
newtype NonStarTypeVarTest (f :: * -> *) a = NSTVTest (f a)

-- >>> :k NonStarTypeVarTest
-- NonStarTypeVarTest :: (* -> *) -> * -> *
```

さて、先ほど定義した `List` にIntが入っていてもMyBoolが入っていても同じように使える「多相関数」を定義してみましょう
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

-- >>> lIntTest
-- >>> lLength lIntTest
-- LCons 1 (LCons 2 (LCons 3 LNil))
-- 3

-- >>> lBoolTest
-- >>> lLength lBoolTest
-- LCons 1 (LCons 2 (LCons 3 LNil))
-- 2
```

最後に、「計算に失敗して値が返ってこないことがある」ということを表す、RustのOption型を移植してみましょう
```haskell
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
```

Haskellの標準ライブラリや、第3章でつかうような様々なライブラリには、data宣言で定義された型がたくさん含まれています。たとえば、
```haskell
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
```

#### 節末問題
1.  `IntList` や `List a` どうしを結合する関数、`ilappend :: IntList -> IntList -> IntList` や `lappend :: List a -> List a -> List a` を定義してみよう
2.  `List a` の各要素に型 `a -> b` の関数を適用する関数 `listmap :: (a -> b) -> List a -> List b` を定義してみよう

### 1.5 型クラス
- ghci上で `:t 42` としたときに表示された `Num a => a` とは、あるいは、`(+) :: Num a => a -> a -> a` とは、どういう意味だろうか？
  - 型シグネチャに `=>` が出てきた場合、左側は「型クラス制約」と呼ばれる型に関する条件をあらわすもので、右側が型本体である
  - ここで出てきた `Num` は、「型クラス」と呼ばれるものの一つで、述語論理における「述語」のようなものである
  - `Num` の具体的な意味は「和・差・積・絶対値・符号の取り出し・整数からのキャスト ができる」である
  - 制約 `Num a` を満たす具体的な型として、`Int` や `Double` がある

- 型クラスは、Javaのinterfaceに類似した概念（型クラスのほうが表現力が高い）であり、Rustにおけるtraitの元ネタである
- 型クラスのKindを調べると、たとえば `Num :: * -> Constraint` と出てくる
  - `Constraint` は、型クラス制約を表すKindであり、型クラスのKindは `-> Constraint` で終わる

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


#### 節末問題
- `(\f x y -> f y x)` という関数は、標準ライブラリでは何と呼ばれているだろうか？
  - ヒント：これの型を考えて、Hoogleで検索し、ドキュメントを読んでほんとうにそう振る舞うか確認しよう

- 型 `V1D`・`V2D`・`V3D` を次のように定義した。ノルム空間を表す型クラス `RNorm` を定義し、これらの型が `RNorm` を満たすようにしてみよう
```haskell
data V1D = V1D Double
data V2D = V2D Double Double
data V3D = V3D Double Double Double
```

### 1.6 おまけ - 便利なサイト・関数・構文 (その1)
- 第1章の中に入れたかったが、ちょうどよい場所の見つからなかった便利な事柄について、この節でいくつか紹介する
- [Hoogle](https://hoogle.haskell.org) は、

## 第2章 モナドと和解する
### 2.1 mapができるもの、Functor
- 他の言語で、配列に対するmapをしたことはあるだろうか
- 1.4の節末問題で扱ったとおり、Haskellで定義したリストについても同じようにmapをすることができる
```haskell
listmap :: (a -> b) -> List a -> List b
listmap f x =
  case x of
    LNil -> LNil
    LCons y ys -> LCons (f y) (listmap f ys)
```

- あるいは、Rustを使っている際、Option に map をしたことがあるだろうか
- Haskellでは、次のような Just の時だけ中身を変換する関数を定義することができる
```haskell
maymap :: (a -> b) -> Maybe a -> Maybe b
maymap f x =
  case x of
    Nothing -> Nothing
    Just y -> Just (f y)
```

- 同様の「中身に関数を適用する」操作は、他の型に対しても定義することができそうである

- Haskellで頻繁に使う型クラスの一つに、このような操作ができることを表す型クラス `Functor` がある
  - 型クラス `Functor` のKindを調べると `(* -> *) -> Constraint` と出てくる
    - つまり、`Functor` は `List` とか `Maybe` のような型一個を受け取って具体的な型になるやつらに関する型クラスである
  - `f` が `Functor f` を満たすなら、`fmap :: forall a b. (a -> b) -> f a -> f b` という関数を持つ
    - `[]` とか `Maybe` は `Functor` なので、`fmap` を使うことができ、それぞれ上と同じように定義されている
  - `Functor` は圏論（数学の一分野）における「自己関手」の概念に由来する
    - この定義に従って、自分で定義した型を `Functor` のインスタンスとする際には次の条件を満たすことがお願いされている
      - `fmap id = id` （恒等射の保存）（ここでいう `id` とは、`\x -> x` のこと）
      - `fmap (f . g) = fmap f . fmap g` （射の合成の保存）（ここでいう `.` は関数合成の演算子、`\f g -> (\x -> f (g x))` のこと）
    - プログラマがコードを書く際にはこの条件が当然満たされているものだと信じている
      - まっとうなライブラリの`Functor`ならば`fmap`はこの条件を満たすように実装されているし、それができないなら`Functor`のインスタンスを実装していないと思う
      - 我々が`Functor`のインスタンスを実装する際も、当然マナーとしてこの条件が満たされるべきである

- `Functor` の中には、「中身」という言葉が上手く使えないために `fmap` を「中身に関数を適用する関数」と説明できないものもある
```haskell
-- このような IntTo について「型aをもつ中身」を考えるのは無理そうである
data IntTo a = IntTo (Int -> a)

instance Functor IntTo where
  fmap f (IntTo g) = IntTo (\x -> f (g x)) -- が、実際これはFunctorについてお願いされた条件を満たしている
```
- 極端な例として、マジで中身のない型についても `Functor` にできる
```haskell
data Phantom a = Phantom -- どのような型aについても、Phantom a の値は Phantom ただ一つである

instance Functor Phantom where
  fmap f Phantom = Phantom -- これはお願いされた条件を満たす
```

- `Functor` の実際の用途として、配列様のものへのmapを全部`<$>`で書くのが一番頻繁かつ便利な用途である気はする
```haskell
doubleAll :: [Int] -> [Int]
doubleAll x = (\y -> y * 2) <$> x -- <$> は fmap の中置演算子版。めっちゃ便利
```

- `Functor` より上等な機能を持っているやつらのための型クラスとして、n引数の関数でもmapのようなことができる `Applicative` がある
  - `Applicative f` は、`Functor f` より狭い制約なので、定義は `class Functor f => Applicative f where` で始まる
    - この`=>`（制約の広い狭い、なのかな）は型クラス制約の`=>`とはちょっと意味が違う気がするので注意
  - `Applicative f` ならば、`fmap` に加えて `pure :: a -> f a` と `<*> :: f (a -> b) -> f a -> f b` が使えるようになる
  - `Applicative` のインスタンスを実装する際には、次の条件を満たすことがお願いされている：
    - `pure id <*> v = v`
    - `pure (\f g -> (\x -> f (g x))) <*> u <*> v <*> w = u <*> (v <*> w)`
    - `pure f <*> pure x = pure (f x)`
    - `u <*> pure y = pure (\f -> f y) <*> u`
```haskell
-- []の場合、pure・<*>(別名ap) は次のような中身である
pureList :: a -> [a]
pureList x = [x]

apList :: [a -> b] -> [a] -> [b]
apList fs xs =
  case fs of
    [] -> []
    f : fs' -> (f <$> xs) ++ (fs' <*> xs)

-- 

addOverList :: [Int] -> [Int] -> [Int]
addOverList lx ly = (+) <$> lx <*> ly 
{-
これの型はちょっとややこしいので分解して考える
まず、
  (+) :: Int -> (Int -> Int) 
  (<$>) :: (a -> b) -> f a -> f b （ここで、fは具体的にはリストなので、(a -> b) -> [a] -> [b] と思ってよい）
  lx :: [Int]
なので、
  ((+) <$> lx) の型は [Int -> Int] となる
次に、
  (<*>) :: f (a -> b) -> f a -> f b （ここで、fは具体的にはリストなので、[a -> b] -> [a] -> [b] と思ってよい）
  ly :: [Int]
なので、
  ((+) <$> lx <*> ly) の型は [Int] となる
-}

-- 当然こういう抽象化ができる
keisanOverList :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
keisanOverList f lx ly = f <$> lx <*> ly

-- もっと抽象化できる
opOverApplicative :: Applicative f => (a -> a -> a) -> f a -> f a -> f a
opOverApplicative f lx ly = f <$> lx <*> ly

-- ここまで抽象化したらプログラム中のいろいろな部分で opOverApplicative を使えそうな気がしてくる
-- これが、Haskellにおける「型クラス」の真の力である
```

- 最後に、`MyList` を `Functor`・`Applicative` にしてみよう
```haskell
instance Functor MyList where
  fmap f xlist =
    case xlist of
      MyNil -> MyNil
      MyCons x xs -> MyCons (f x) (fmap f xs)

instance Applicative MyList where
  pure x = MyCons x MyNil
  (<*>) flist xlist =
    case flist of
      MyNil -> MyNil
      MyCons f fs -> (f <$> xlist) `myappend` (fs <*> xlist)
```

#### 節末問題
- `MyList`・`IntTo`・`Phantom` について、それぞれお願いされた条件を満たしていることを確かめよう

### 2.2 連鎖ができるもの、Monad
- `Maybe` は革命的に便利な型であるが、`Functor`や`Applicative`だけだとその力を最大限に引き出すことはできない
- リスト内で間接参照をする例を考えてみよう
```haskell
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
      -- ↑ ガード節と呼ばれる、パターンマッチ後に条件を書くことができる便利な構文

-- indirectTwice l x という、リストlの「リストlのx番目の数字」番目を返す関数を作りたい
-- 例：indirectTwice [2, 0, 1] 1 = Just 2（[2, 0, 1]の1番目は0なので、[2, 0, 1]の0番目の数字2を取り出せて、Just 2 が帰ってくる、ようにしたい）
indirectTwice l x = (l !?) <$> (l !? x)
--                   ↑ これは、セクションと呼ばれる構文。丸括弧のなかに中置演算子と値片方のみを突っ込むと、残りの値を待つ関数になる

-- 残念ながら、このindirectTwiceの型は [Int] -> Int -> Maybe (Maybe Int) となってしまう
```
- 仕方がないので、入れ子になった `Maybe` をつぶして「平ら」にする関数 `flatten :: Maybe (Maybe a) -> Maybe a` を定義してみることにする
```haskell
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
indirectTwice2 l x = flatten ((l !?) <$> (l !? x))
```
- あるいは、最初から and_thenみたく `flatmap :: (a -> Maybe b) -> Maybe a -> Maybe b` という関数を定義したほうがわかりやすいかもしれない
```haskell
-- この mayflatmap は flatten . fmap と等価である
mayflatmap :: (a -> Maybe b) -> Maybe a -> Maybe b
mayflatmap f x =
  case x of
    Nothing -> Nothing
    Just y -> f y

-- これを使って、indirectTwiceを書き直す
indirectTwice3 l x = (l !?) `mayflatmap` (l !? x)
```
- こう書くと、`(l !?)` に `(l !? x)` を注入しているみたいだから、そのイメージをかっこよく表した中置演算子を定義してみよう
```haskell
-- ろうとの形だと思ってほしい
(=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
(=<<) = mayflatmap

-- 先に計算するのは Maybe a のほうなのだから、逆向きのやつもほしい
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) = flip mayflatmap
```
- 名前の衝突で怒られてしまったので、いま書いた演算子二つは消しておこう（標準ライブラリの同名の演算子はおなじ挙動をする）
- これを使えば、`indirectThrice` も `indirectQuince` も簡単に書けそうである
```haskell
indirectThrice l x = (l !? x) >>= (l !?) >>= (l !?)
indirectQuince l x = (l !? x) >>= (l !?) >>= (l !?) >>= (l !?)
```

- どうやらこの `and_then` ないし `flatmap` みたいな操作は、`Maybe` の快適な利用に必須のようである

- ところで、`flatmap`というのは他言語では配列のメソッドだった
```haskell
listflatmap :: (a -> [b]) -> [a] -> [b]
listflatmap f x =
  case x of
    [] -> []
    y : ys -> f y ++ listflatmap f ys

-- どうやら標準ライブラリでは、 >>= はリストについては flip listflatmap のように実装されているらしい
```
- `liftflatmap`も使ってみよう
```haskell
{-
元ネタ：ABC049C - 白昼夢 / Daydream
https://atcoder.jp/contests/abs/tasks/arc065_a

Tを空文字列の状態から始め、dream dreamer erase eraser のいずれかを追加する操作をn回行った文字列を全部列挙してみる
-}
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
dreamN n x = dream1 x >>= dreamN (n - 1)

-- >>> dreamN 2 ""
-- ["dreamdream","dreamdreamer","dreamerase","dreameraser","dreamerdream","dreamerdreamer","dreamererase","dreamereraser","erasedream","erasedreamer","eraseerase","eraseeraser","eraserdream","eraserdreamer","erasererase","erasereraser"]
```

- このような `and_then` とか `flatmap` と呼ばれる操作は非常に便利であるため、これができる型のための型クラス `Monad` が存在する
  - `Monad` は `Applicative` のさらに高機能版である
  - `Monad f` を満たす型 `f` については、 `Applicative` で使えた `fmap`・`pure`・`<*>` に加えて、`(>>=) :: f a -> (a -> f b) -> f b` が使えるようになる
  - `Monad` のインスタンスを実装する際には、次の条件を満たすことがお願いされている：
    - `pure x >>= f = f x`
    - `m >>= pure = m`
    - `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`

#### 節末問題
- `MyList` を `Monad` にしてみよう
- `indirectTwice` を参考に、次のような関数を定義しよう
  - `indirectPlus :: [Int] -> Int -> Maybe Int`
  - `indirectPlus l x` は、
    - `l` の `x` 番目の数字を `y` とする
    - `l` の `y` 番目の数字を `z` とする
    - `l` の `y + z` 番目の数字を `w` とする
    - ここまで上手くいったら `Just w` を、さもなくば `Nothing` を返す

### 2.3 do記法でのびやかにモナドを使う
  