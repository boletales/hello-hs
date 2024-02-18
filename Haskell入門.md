# Haskell入門

## 第0章 はじめに
### 0.1 モチベーション
このドキュメントでは、第1章・第2章でHaskellの環境構築と基本的な文法を、第3章で実用的なプログラムを書くために必要なツール群・ライブラリについて学び、最終的的にパーサコンビネータライブラリを用いたある程度実用的なプログラムを書くことを目標とします。

このドキュメントの読者として、プログラミング言語の以下のような機能に慣れ親しんでいる人を想定しています：
- 再帰呼び出し
- 静的型付け
- 無名関数ないしラムダ式
- 高階関数（無名関数を引数に取ったり返したりする関数のこと、配列に対する`map`とか）
- パラメータのついた型・ジェネリクス（C++の`vector<T>`とか）

Haskellは「静的型付き純粋関数型プログラミング言語」である、つまり、静的に型の付く「純粋な関数」を用いて「関数型プログラミング」と呼ばれるスタイルのプログラミングをすることに特化した言語である、と説明されることがあります。「純粋な関数」とか「関数型プログラミング」とは何で、どのような恩恵をもたらしてくれるのでしょうか。

プログラミングの文脈において、「関数」という言葉は「いろんな命令を実行して最後に値を返す手続き」と「値を変換する写像」の二つの側面を持ちます。
```C
// Cのコード
int someGlobalVar = 0;

// 関数 as 手続きの流れを切り出したもの（値が返ってくることもある）
int procedure(int x) {
  someGlobalVar += x;
  printf("%d\n", someGlobalVar);
  return someGlobalVar;
}

// 関数 as 値を受け取って値を返すもの（手続きをすることもある）
int converter(int x) {
  return x * 2;
}
```
関数型プログラミングという言葉に明確な定義はありませんが、端的に言えば関数の「写像」としての側面を多用するスタイルのプログラミングのことを指します。

プログラムの仕様を説明する際に「○○を××したもの」という表現が多く出てくるようであれば、関数型プログラミングの恩恵を強く受けられます。極端な例として、Pythonで書かれた次のコード片を見てみましょう。
```python
# ある配列から、その各要素を二乗したのち30より小さいものだけ取り出した配列を作る、Pythonのコード

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

関数型プログラミング言語を習得していない人は、仕様の中に「△△は○○を××したものである」という表現が増えれば増えるほど、翻訳調の手続き型プログラミングと快適でない関数型プログラミングの間で選択を迫られることになります。

これは無用な苦痛です。関数型プログラミングが上手く填まる場面にはHaskellのような関数型プログラミング言語を持ってくるべきなのです。
```haskell
-- これがHaskellのコードです
squareThenFilterUnder30 :: [Int] -> [Int]
squareThenFilterUnder30 = filter (< 30) . map (^ 2)
```
「関数型プログラミング言語」という言葉にも明確な定義はありませんが、おおむね「関数型プログラミングが快適」「関数を見て『写像』だと納得しやすい」という特徴を言語仕様で実現しているプログラミング言語がこう呼ばれます。

Haskellの特徴について軽く紹介します：

1. 関数は「ふつうの値」であり、変数に入れたり、引数として渡したり、返り値として返したりできる
   
    これは、現代的なプログラミング言語の間では広く普及した特徴かもしれません。関数型プログラミングを快適にするためには必須です。

2. すべての関数は「純粋」であり、いかなる場合でも関数適用はその値に置き換えても構わない

    これはHaskellをはじめとした「純粋関数型プログラミング言語」だけの特徴です。「関数が純粋」は他の言語で例えるならば、「`hoge(42) == 100` が成立するならいつだって`hoge(42)`の値は`100`なのだから、プログラム中の `hoge(42)` を全部 `100` に置換しても全く問題ない」という意味です。Haskellではすべての関数についてこれが成り立っているので、コンパイラはかなりアグレッシブに定数畳み込みのような最適化を行うことが許されます。

3. すべての式にはコンパイル時点で型が付いていて、安全なNullやエラーのための型がある

    冒頭に一瞬出てきた「静的型付き」の部分のことです。Haskellの文化では、TSとかRustのようにとにかくカジュアルに型を定義して、いろいろなことを型で表現しようとします。代数的データ型とパターンマッチは早くすべてのプログラミング言語に輸入されるべきです。

4. 他の言語でいう副作用は「IOアクション」とよばれる種類の値として表現される
  
      「IOアクション」というのは、「値を返す手続きの『やり方』」のことで、JavaScriptで例えれば「勝手に発火しないし入れ子にもできるお行儀のよいPromise」のようなものです。Haskellにおいて、プログラマの仕事は、エントリポイントとなる一つの大きなIOアクションを組み立てることです。（JSを触ったことがある皆さんはご存じの通り、）手続きを組み立てることとそれが実行されることを分離して考えるのは、非同期やマルチスレッドの要素を含むWebサーバーのプログラムを書く上でとても有益です。

5. コンパイラによる強烈な最適化が効きさえすれば、同じ課題についてJavaぐらいの速度（実行時間がCの3倍で済む程度）で動作する

    「同じ課題について」であって、「同じように関数型の書き方をしたコードについて」ではないことに注目してください。Haskellで書いたプログラムは、関数型プログラミングに最適化されていない言語で同じ関数型の書き方をしたプログラムよりはるかに高速に動作します。

6. 「関数を組み合わせるための関数」がめちゃくちゃ豊富に用意されている

    「ふつうのプログラミング言語」で関数型プログラミングをする際の不愉快な点の一つは、小さい関数をたくさん書くとそれを組み合わせた式がまどろっこしい見た目になることです。Haskellには大量の中置演算子（mapとか、関数合成とか、モノイドの結合とか）が用意されており、ユーザーが新しく定義することもできます。

7. パフォーマンスが予測しづらく（たいていJSよりは速い）、メモリ管理はGCに一任されている

      コンパイラがマジカルな最適化を多数行うため、パフォーマンスを改善したければプロファイリングが必須です。また、残念ながら、値の不変性やモナドのような激強構造はかっちりとしたメモリ管理とは相性がよくないようです。厳密なメモリ管理やガチガチのパフォーマンスチューニングを求められるような用途であれば、おとなしくRustを使ってください。

これらの特徴がうまくかみ合うような場面でHaskellを使えば高い生産性を実現できます。コンパイラや自作言語の実装においてHaskellの右に出る言語はおそらくないですし、Webサーバーのような副作用の流れが複雑な場面でも優秀です。上の特徴にがっつりかみ合う場面でなくても、普段使い用のまともに型が付いてスクリプトっぽい書き味が実現できる言語、語弊を恐れずに言えばPythonしか知らない人のPython代替としてそれなりの使いやすさがあります。（実際に、GHCのインタプリタモードを使えばスクリプトとして実行できます）

逆に、これらの特徴が最悪の方向にかみ合うような場面、つまるところコードと機械語の対応の明白さや、メモリ管理の厳密さが求められるような場面には向いていないので、そういうときにはおとなしくRustを使ってください。

最後に、「Haskellには副作用がないからHello Worldにすら尋常でない労力を要する」という都市伝説を払拭するために、IOを使った短いコードを置いておきます。

```haskell
-- 文字列から「た」を抜く関数
tanuki :: String -> String
tanuki = filter (\c -> c /= 'た')

-- 一行読んで、たを抜いて、表示する
main :: IO ()
main = putStrLn =<< (tanuki <$> getLine)

-- ふつうの言語っぽい見た目でも書ける
-- 注意：doとかいうやつのことは、第2章で説明するまできれいさっぱり忘れておくこと！
main2 :: IO ()
main2 = do
  line <- getLine
  putStrLn (tanuki line)

-- ↓Haskell Language Serverとエディタのプラグインを導入すれば、こういうコメントを書いてエディタ上で式の評価結果を確認できる
-- >>> tanuki "こたんたたにたちたは"
-- "こんにちは"
```


### 0.2 環境構築
2024年初頭の現時点では、ツールチェーンマネージャの [GHCup](https://www.haskell.org/ghcup/) を使うことが推奨されています。インストールの途中で「Stackをインストールするか？」「HLS(Haskell Language Server)をインストールするか？」と訊かれますが、これにはどちらもyと答えてください。Windowsを使っている場合は、インストール後に環境変数の変更を反映させるため、一度再起動してください。

インストールされるツール群は以下の通りです：
- GHCup
- GHC（Haskellのコンパイラ）
- Cabal（Haskellのパッケージマネージャ）
- Stack（Cabalのラッパー）
- HLS（Haskell Language Server、エディタ上での補完などをしてくれる）

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

このドキュメントでは、この先しばらく（第2章のなかごろまで）は、一枚のファイルとそれを読み込んだghciで進めていくことにします。ファイルを編集したら、忘れずに `:r` するようにしてください。


#### 節末問題
無からコードを起こす練習をするため、CopilotなどのAI補完を使っている場合は節末問題や章末問題ではオフにすることをお勧めします。
1．ここまでの知識だけで、「二個の引数を取る関数」を作ることはできるでしょうか？
  - たとえば、`(kakezan 3) 4` と打つと、`12` が返ってくるような関数`kakezan`を作ってみてください
  - ヒント：関数はふつうの値の一種なので、関数を関数の結果として返すことができます

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
Haskellで条件分岐をする場合には、`case`式とパターンマッチを使います。Haskellはオフサイドルールと呼ばれる、ブロックや式の範囲をインデントで表す構文規則を採用しているため、コピーペーストをする際には注意が必要です（オフサイドルールを採用している他の言語として、Pythonなどがあります）。
```haskell
collatz :: Int -> Int
collatz x =
  -- 関数をバッククオートで囲むと、中置演算子として使える
  -- x `mod` 2 と mod x 2 は同じ意味
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

case式のどのパターンにも当てはまらなかった場合、関数が例外を吐いて落ちます。これは「部分関数」と呼ばれ、ろくな代物ではないため自分で書くことは避けてください。書こうとしている関数の定義域が型の値全体にならないような場合には、次節で触れる「Maybe」という型を使うのがHaskellらしいよい文化です。
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

-- 型名（左辺）と値コンストラクタ（右辺）名は重複してよい
data ISBN = ISBN Int
  deriving Show

-- newtype は、別の型のラッパー（値コンストラクタが1種類で、その引数が1つであるとき）専用のdata宣言のようなもの（dataでもよいが、newtypeはコンパイル時に削除されるため、パフォーマンス上での利点がある）
newtype Price = Price Int
  deriving Show

-- Int, Price, ISBN は相互に区別される
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
data NonStarTypeVarTest (f :: * -> *) a = NSTVTest (f a)

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

-- () は、唯一の値として () を持つ型。2.1節以降で出てくる Functor などと組み合わせて使われることが多い。　() は以下と等価：
data Unit = Unit

theOnlyOneValueOfUnit :: ()
theOnlyOneValueOfUnit = ()
```

#### 節末問題
1.  `IntList` や `List a` どうしを結合する関数、`ilappend :: IntList -> IntList -> IntList` や `lappend :: List a -> List a -> List a` を定義してみましょう。
2.  `List a` の各要素に型 `a -> b` の関数を適用する関数 `listmap :: (a -> b) -> List a -> List b` を定義してみましょう。
3.  整数のリストをソートする関数 `mysortint :: [Int] -> [Int]` を定義してみましょう。
      - リストをソートするアルゴリズムは複数考えることができますが、リストの構造に適したアルゴリズムはどれでしょうか？
      - 同様のアルゴリズムを用いた手続き型のコードと比較して、どのような違いがあるでしょうか？
      - `mysortint` はコードをほとんど書き換えずにより一般的な型に対して定義することができます。その場合、どのような型シグネチャになるでしょうか？

### 1.5 型クラス
1.1節で少し遊んでいた場合、あるいはここまでで型エラーを見た場合、型シグネチャの中にここまで説明されなかった `=>` という記号を見たのではないでしょうか。
```
ghci> :t 42
42 :: Num a => a
ghci> :t (+)
(+) :: Num a => a -> a -> a
```
型シグネチャにおいて、`=>` の右側は型そのものを、左側は型に関する条件を表します。`Num` は、この条件（型クラス制約と呼ばれます）を書くための「型クラス」と呼ばれるものです。

実際に型クラスを定義してみましょう。型クラスを定義する際、その型クラスに型を属させるにあたって特定の関数の実装を要求することができます。
```haskell
-- [elem] と相互変換できるものたちの型クラス
class Listish elem listish where
  toList :: listish -> [elem]
  fromList :: [elem] -> listish
-- ある elem, listish について、 Listish elem listish としたければ、toList と fromList を実装すること！

-- 型クラスの引数を埋めると、Constraintという型クラス制約を表すKindが出てくる
-- >>> :k Listish
-- Listish :: * -> * -> Constraint

-- IntList と [Int] は相互変換できるので、Listishに属させてみる（）
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
```
型クラスは、Javaのinterfaceに類似した概念（型クラスのほうが表現力が高い）であり、Rustにおけるtraitの元ネタです。

型クラスと型クラス制約を用いて、`Listish`に属する型全般に使える関数を定義してみます。
```haskell
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
```

「これを定義せよ」と指定された関数さえすべて実装すればどんな型でもインスタンスにできますが、多くの型クラスは「インスタンス宣言で指定された構造が特定の性質を持つ」ことを意図して作られています（モノイドであれば単位律と結合律、リストとの相互変換だったら `toList` と `fromList` の合成が恒等関数になること、など）。意図された性質（「則」とか「Law」などと呼ばれます）を満たさないインスタンス宣言は避けるべきですし、「則」を持たない型クラスをむやみやたらに定義すればコードの可読性を損ねることに注意する必要があります。

Haskellの標準ライブラリや、この先用いるライブラリ様々には数多くの型クラスが定義されています。標準ライブラリで定義された型クラスのうち、頻繁に使うものには次のような例があります。 
- `Eq a`      : `a`の等価比較ができる
- `Ord a`     : `a`の順序比較ができる
- `Show a`    : `a`は`String`に変換できる
- `Read a`    : `a`は`String`から変換できる
- `Num a`     : `a`は和・差・積・絶対値・符号の取り出し・整数からのキャスト ができる
- `Monoid a`  : `a`はモノイドである
- `Functor f` : `f :: (* -> *)` について、配列に対するmapと同じような`fmap :: (x -> y) -> f x -> f y`ができる（第2章のメインテーマです）

ここで挙げたうち、`Eq`・`Ord`・`Show` にはデフォルト実装が用意されており、自分で型を定義したあと `deriving (Show, Eq)` と書けば勝手にインスタンス定義を行ってくれます。

Haskellにおける`String`は、`[Char]`のエイリアスです。（パフォーマンス上の欠点が多いため、実用のプログラムで文字列を表す際には第3章で触れる`Text`という型がよく用いられます）
```haskell
data Stars = Stars Int
instance Show Stars where
  show (Stars n) = 
    case n of
      0 -> "" -- ['']と同義
      _ -> '*' : show (Stars (n - 1))
```


#### 節末問題
1. 型 `V1D`・`V2D`・`V3D` を次のように定義します。実線形空間を表す型クラス `RVect` を定義し、これらの型をそのインスタンスにしましょう。
```haskell
data V1D = V1D Double
data V2D = V2D Double Double
data V3D = V3D Double Double Double
```

### 1.6 便利なサイト・関数・構文
第2章に入る前に、便利なサイトや関数・構文をいくつか紹介しておきます。

[Hoogle](https://hoogle.haskell.org) は、型や名前を入力して関数を検索できるすごいサイトです。`set:stackage`となっている部分を`set:included-with-ghc`に変えて、`[a] -> Int`と入力すると、トップに次のような結果が出てきます。
```
length :: Foldable t => t a -> Int
base Prelude Data.List Data.Foldable
```
検索結果をクリックすると、ドキュメントを読むことができます。`Foldable` は畳み込み（JSのArray.prototype.reduceなど）をサポートしているものたちの型クラスで、リストはこれに含まれています。二行目の`base Prelude Data.List Data.Foldable`は、この関数が含まれているパッケージ（`base`）とモジュール（`Prelude`・`Data.List`・`Data.Foldable`）を表しています。`Prelude`の関数は、インポートなしで使うことができます。

`base`やGHCに同梱されたパッケージ内のモジュールは、ファイルの先頭に `import Put.Module.Name.Here` と書くことで使うことができます。
```haskell
import Data.List (sort)

-- >>> sort [4, 3, 1 ,2]
-- [1, 2, 3, 4]
```

また、便利な関数や構文についてまとめて紹介します：
```haskell
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
aliceOneYearLater :: Person
aliceOneYearLater = alice { age = age alice + 1 }


-- 型シノニム：型に別名をつける（String2と[Char]は同一の型として扱われます）
type String2 = [Char]

-- タプル：複数の型の直積
intAndBool :: (Int, Bool)
intAndBool = (3, True)
```

#### 節末問題
1. `(\f x y -> f y x)` という関数は、標準ライブラリでは何と呼ばれているでしょうか？

### 1.7 章末問題：関数型プログラミングの練習（歯ごたえマシマシなので問題を変えるかも）
ここまで、Haskellの基本的な文法について解説してきましたが、世の中に存在する問題に関数型プログラミングで立ち向かうには慣れが必要です。本節では、練習として1章までの内容で次のような問題を解いてみましょう。（追記とデバッグをやりやすくするために、`fare.hs`のような別のファイルを用意するのがよいと思います。）

1.1にも書きましたが、無からコードを起こす練習をするため、CopilotなどのAI補完を使っている場合は節末問題や章末問題ではオフにすることをお勧めします。

以下のようなキロ程表・運賃表のcsv文字列と、駅名二つを与えられたときに、その駅間の運賃を計算するプログラムを書いてみましょう。
ただし、路線は二つあり、運賃計算の関数をそれぞれの路線に対して用意したいものとします。
```
駅名, キロ程
本町, 0.0
市役所前, 0.7
城址公園, 1.3
運動場前, 2.0
大学北, 2.7
みなと広場, 3.3
県営団地, 4.0
温泉入口, 4.7
登山口, 5.3
```
```
駅名, キロ程
空港前, 0.0
工業団地, 1.2
市場前, 1.9
駅北, 2.5
駅南, 2.7
大橋, 3.5
```
```
運賃, 最小キロ程
130, 0.0
170, 1.0
210, 2.0
250, 3.0
290, 4.0
330, 5.0
```
入出力の方法は第二章の中盤で触れますが、ここでは説明を省略するために、それぞれの文字列が入った変数を定義しておきます。
```haskell
hommachiLineCSV :: String
hommachiLineCSV = "駅名, キロ程\n本町, 0.0\n市役所前, 0.7\n城址公園, 1.3\n運動場前, 2.0\n大学北, 2.7\nみなと広場, 3.3\n県営団地, 4.0\n温泉入口, 4.7\n登山口, 5.3"

kukoLineCSV :: String
kukoLineCSV = "駅名, キロ程\n空港前, 0.0\n工業団地, 1.2\n市場前, 1.9\n駅北, 2.5\n駅南, 2.7\n大橋, 3.5"

faresCSV :: String
faresCSV = "運賃, 最小キロ程\n130, 0.0\n170, 1.0\n210, 2.0\n250, 3.0\n290, 4.0\n330, 5.0"
```
このデータを用いて運賃計算をするための関数 `calcFareAndKiloFromCSV` を書くことはできるでしょうか？
```haskell
calcFareFromCSV :: String -> String -> (String -> String -> Maybe (Int, Double))
calcFareFromCSV stationsCSV faresCSV =
  (\from to ->
      Just 0
    )  -- ← ここを適切な式に書き換える
```

使えそうな関数をヒントとして挙げておきます。
```haskell
-- なぜかPreludeに入っていない文字列の分割
splitOn :: Char -> String -> [String]
splitOn c str =
  case break (== c) str of
    (x, "") -> [x]
    (x, _:ys) -> x : splitOn c ys

-- 改行で分割（splitOn '\n' と同じような挙動だったと思います）
-- lines :: String -> [String]

-- 文字列の先頭部分を他の型へパース（返り値：パース候補と残りの文字列の組のリスト。IntとDoubleはそれぞれRead型クラスに属するため、readsが使えます）
-- reads :: Read a => String -> [(a, String)]

-- readsの結果が空でなかったら先頭を返す
readMay :: Read a => String -> Maybe a
readMay str =
  case reads str of
    [(x, "")] -> Just x
    _ -> Nothing

-- 先頭から、特定の条件を満たす要素を取り除きつづける
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- 例： dropWhile (== ' ') "   abc" == "abc"

-- 先頭から、特定の条件を満たす要素を取り出し続ける
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- 例： takeWhile (/= ' ') "abc " == "abc"

-- Maybeにくるまれた値をMaybeを返す関数に渡す（この関数はPreludeの >>= とほぼ同じで、2章のどこかで再登場します）
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen mayx f =
  case mayx of
    Nothing -> Nothing
    Just x -> f x

-- Maybe aのリストをまとめる。リストの要素を一個ずつ見ていって、全部JustだったらJustにくるまれたリストを返す（この関数はPreludeの sequenceA とほぼ同じで、2章のどこかで再登場します）
sumupMaybes :: [Maybe a] -> Maybe [a]
sumupMaybes maylist =
  case maylist of
    [] -> Just []
    Nothing : _ -> Nothing
    Just x : maylist' -> 
      case sumupMaybes maylist' of
        Nothing -> Nothing
        Just xs -> Just (x : xs)

-- リストの先頭から関数を適用していって、途中でNothingが出てきたらやめる（この関数はPreludeの traverse とほぼ同じで、2章のどこかで再登場します）
sumupMayMap :: (a -> Maybe b) -> [a] -> Maybe [b]
sumupMayMap f list =
  case list of
    [] -> Just []
    x : xs ->
      case f x of
        Nothing -> Nothing
        Just y -> 
          case sumupMayMap f xs of
            Nothing -> Nothing
            Just ys -> Just (y : ys)
  -- sumupMaybes (map f x) とおなじ
```
<details>
<summary>また、この問題を解くには次のような関数を用意すればよさそうです（見ずに解いた方が練習になりそうであるため、折りたたみ）</summary>

```haskell
-- readCSV :: String -> Maybe ([String], [[String]])  ← (先頭行, それ以外)
-- readStationsCSV :: String -> Maybe [(String, Double)]
-- readFaresCSV :: String -> Maybe [(Double, Int)]
-- getStationPos :: [(String, Double)] -> String -> Maybe Double
-- kiloToFare :: [(Int, Double)] -> Double -> Maybe Int
-- calcFareAndKilo :: [(String, Double)] -> [(Int, Double)]-> Double -> Double -> Maybe (Int, Double)

-- ただし、readStationsやreadFaresはもっと分解したほうがよさそうです
```
</details>
ここで、コードを書いている際に感じた「Maybeの付け外しが煩わしい」とか「同じ引数を取り回し続けて面倒だ」といった感覚をよく覚えておいてください。第2章でとりあつかう概念や関数は、これらの問題を解決するためのものです。また、先に2章をのぞき見するのも一つの手かもしれません。

解けたら、次のようなコードで挙動を確認してみましょう。
```haskell
calcFareAndKiloHommachi :: String -> String -> Maybe (Int, Double)
calcFareAndKiloHommachi = calcFareAndKiloFromCSV hommachiLineCSV faresCSV

calcFareAndKiloKuko :: String -> String -> Maybe (Int, Double)
calcFareAndKiloKuko = calcFareAndKiloFromCSV kukoLineCSV faresCSV

-- >>> calcFareAndKiloHommachi "本町" "登山口"
-- Just (330, 5.3)
```

## 第2章 モナドと和解する
### 2.1 「中身に適用」ができるもの、Functor
「配列様のデータの各要素に関数を適用する」操作は、プログラミングの場面において頻出します。1.3節で定義した `List` について、この操作を行う関数を書いてみます（1.3節の章末問題で出てきた `listmap` です）。
```haskell
-- 参考: map :: (a -> b) -> [a] -> [b] 
-- リストのそれぞれの要素にfを適用した新しいリストを返す
listmap :: (a -> b) -> List a -> List b
listmap f x =
  case x of
    LNil -> LNil
    LCons y ys -> LCons (f y) (listmap f ys)
```

`Maybe`についても、「中身に関数を適用する」操作をおなじように考えることができます。これは、RustのOption型における`map`に相当します。
```haskell
maymap :: (a -> b) -> Maybe a -> Maybe b
maymap f x =
  case x of
    Nothing -> Nothing
    Just y -> Just (f y)
```

`map`・`listmap`・`maymap` の型シグネチャを見比べてみると、それぞれなんらかの型コンストラクタ `f :: (* -> *)` について `(a -> b) -> (f a -> f b)` という形の型をしていることがわかります。

リストやMaybeをはじめとした、中身に関数を適用するような操作ができる型に対して、Haskellでは`Functor (f :: * -> *)`という型クラスが用意されています。Hoogleで`Functor`と検索し、検索結果をクリックしてドキュメントを読んでみましょう。

> class Functor (f :: Type -> Type) where
> 
> A type f is a Functor if it provides a function fmap which, given any types a and b lets you apply any function from (a -> b) to turn an f a into an f b, preserving the structure of f. Furthermore f needs to adhere to the following:
> 
> - Identity
>     fmap id == id
> - Composition
>     fmap (f . g) == fmap f . fmap g
> 
> Note, that the second law follows from the free theorem of the type fmap and the first law, so you need only check that the former condition holds. See https://www.schoolofhaskell.com/user/edwardk/snippets/fmap or https://github.com/quchen/articles/blob/master/second_functor_law.md for an explanation.
> 
> Minimal complete definition
> - fmap

`Functor` は、先ほどの `map`・`listmap`・`maymap` に相当する関数 `fmap` の実装を要求する型クラスです。また、`fmap`の実装は以下の性質を満たすことを想定されています。
- `fmap id = id` （恒等射の保存）（ここでいう `id` とは、`\x -> x` のこと）
- `fmap (f . g) = fmap f . fmap g` （射の合成の保存）（ここでいう `.` は関数合成の演算子、`\f g -> (\x -> f (g x))` のこと）

この`Functor`は、数学の一分野である圏論の「自己関手」と呼ばれる概念に由来しますが、これについての詳しい解説は割愛します。

fmapのことを「中身に関数を適用するような操作」と表現しましたが、`Functor` の中には、「中身」という言葉がうまく当てはまらないものも存在します
```haskell
-- このような IntTo について「型aをもつ中身」を考えるのは無理そうである
data IntTo a = IntTo (Int -> a)

instance Functor IntTo where
  fmap f (IntTo g) = IntTo (\x -> f (g x)) -- が、実際このfmapはFunctorの則を満たしている

-- 極端な例として、型変数にかかわらず一種類の値しか持たない Phantom という Functor を考えてみる
data Phantom a = Phantom

instance Functor Phantom where
  fmap f Phantom = Phantom -- このfmapもFunctorの則を満たしている
```

`fmap`の中置演算子版である `<$>` を使うと、コードの見た目をすっきりさせることができます。
```haskell
doubleAll :: [Int] -> [Int]
doubleAll x = (\y -> y * 2) <$> x -- <$> は fmap の中置演算子版。めっちゃ便利
```

`Functor` より高等な機能を持っている型のための型クラスはいくつか存在しますが、ここでは、n引数の関数でもmapのようなことができる `Applicative` というものを紹介します。Hoogleでドキュメントを検索してみましょう。

> class Functor f => Applicative (f :: Type -> Type) where
> 
> A functor with application, providing operations to
> 
> - embed pure expressions (pure), and
> - sequence computations and combine their results (<*> and liftA2).
> 
> （中略）
>
> Further, any definition must satisfy the following:
>  
> Identity
> 
>     pure id <*> v = v
> 
> Composition
> 
>     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
> 
> Homomorphism
> 
>     pure f <*> pure x = pure (f x)
> 
> Interchange
> 
>     u <*> pure y = pure ($ y) <*> u
> 
> （中略）
> 
> Minimal complete definition
> 
> - pure, ((<*>) | liftA2)
> 
> Methods
> 
> pure :: a -> f a
> 
> - Lift a value.
> 
> (<*>) :: f (a -> b) -> f a -> f b infixl 4
> 
> - Sequential application.

`class Functor f => Applicative f where` の `=>` は、制約の広さについての不等号であることに注意してください。`Applicative` は `Functor` であることが要求されています。

`Applicative f` のインスタンス `f` では、`fmap` に加えて `pure :: a -> f a` と `<*> :: f (a -> b) -> f a -> f b` が使えるようになります。また、`Applicative` のインスタンスは、以下の則を満たすことを想定されています。
- `pure id <*> v = v`
- `pure (\f g -> (\x -> f (g x))) <*> u <*> v <*> w = u <*> (v <*> w)`
- `pure f <*> pure x = pure (f x)`
- `u <*> pure y = pure (\f -> f y) <*> u`

`Maybe` や `[]` に対して、`pure` と `<*>` はこのように定義されています
```haskell
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
```
#### 節末問題
1. 次のような「数式」を評価する、`evalMathExpr :: MathExpr -> Maybe Int` を定義してみましょう
```haskell
data MathExpr =
    MEConst Int
  | MEAdd MathExpr MathExpr -- 和
  | MESub MathExpr MathExpr -- 差
  | MEMul MathExpr MathExpr -- 積
  | MEDiv MathExpr MathExpr -- 整数除算（-∞側に切り捨て）、部分関数。0除算の結果は Nothing とする
  | MEMod MathExpr MathExpr -- 整数除算の余り、部分関数。0除算の結果は Nothing とする
  | MEPow MathExpr MathExpr -- べき乗。0^0 と x^負数 は Nothing とする
```

### 2.2 連鎖ができるもの、Monad
`Maybe` は、一度使ったら手放せなくなるぐらいには革命的に便利な型です。しかし、2.1節で出てきた `fmap`や`pure`・`<*>` だけでは、そのポテンシャルを最大限生かすことができません。リストのn番目を読み出す関数をつかって、間接参照をする例を考えてみましょう。
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

-- indirectTwice l x という、リストlの「リストlのx番目の数字」番目を返す関数を作りたい
-- 例：indirectTwice [2, 0, 1] 1 = Just 2（[2, 0, 1]の1番目は0なので、[2, 0, 1]の0番目の数字2を取り出せて、Just 2 が帰ってくる、ようにしたい）
indirectTwiceDup :: [Int] -> Int -> Maybe (Maybe Int)
indirectTwiceDup l x = (l !?) <$> (l !? x)

-- 残念ながら、このindirectTwiceの型は [Int] -> Int -> Maybe (Maybe Int) となってしまう
```
入れ子になった `Maybe` をつぶして「平ら」にする関数 `flatten :: Maybe (Maybe a) -> Maybe a` を定義してみることにします。
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
indirectTwice :: [Int] -> Int -> Maybe Int
indirectTwice l x = flatten ((l !?) <$> (l !? x))
```
このような用途であれば、 `flatmap :: (a -> Maybe b) -> Maybe a -> Maybe b` という関数を定義すれば一発で書くことができます。（このflatmapは、Rustだと`and_then`と呼ばれています）
```haskell
-- この mayflatmap は flatten . fmap と等価である
mayflatmap :: (a -> Maybe b) -> Maybe a -> Maybe b
mayflatmap f x =
  case x of
    Nothing -> Nothing
    Just y -> f y

-- これを使って、indirectTwiceを書き直す
indirectTwice2 :: [Int] -> Int -> Maybe Int
indirectTwice2 l x = (l !?) `mayflatmap` (l !? x)
```
中置記法を用いて書くと、 `(l !?)` という関数に `(l !? x)` の結果を注入しているようなイメージができます。Preludeには、そのイメージを形にしたような中置演算子が定義されています：
```haskell
{-
-- ろうとの形だと思ってほしい
(=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
(=<<) = mayflatmap

-- 先に計算するのは Maybe a のほうなのだから、逆向きのやつもほしい
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) = flip mayflatmap
-}
```

この`=<<`、別名`flatmap`を使えば、間接参照を3回・4回繰り返す `indirectThrice` や `indirectQuince` もシンプルに書くことができそうです。
```haskell
indirectThrice l x = (l !? x) >>= (l !?) >>= (l !?)
indirectQuince l x = (l !? x) >>= (l !?) >>= (l !?) >>= (l !?)
```

ところで、`flatmap`というのは他言語では配列のメソッドでしたから、リストについても定義してみましょう。
```haskell
listflatmap :: (a -> [b]) -> [a] -> [b]
listflatmap f x =
  case x of
    [] -> []
    y : ys -> f y ++ listflatmap f ys
```
標準ライブラリの演算子 `>>=` は、リストについては `flip listflatmap`のようにふるまいます。
```haskell
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
```

リストやMaybeをはじめとした`flatmap`相当の操作ができる型のために、Haskellでは`Monad (f :: * -> *)`という型クラスが用意されています。Hoogleで`Monad`と検索し、検索結果をクリックしてドキュメントを読んでみましょう。

> class Applicative m => Monad (m :: Type -> Type) where
> 
> The Monad class defines the basic operations over a monad, a concept from a branch of mathematics known as category theory. From the perspective of a Haskell programmer, however, it is best to think of a monad as an abstract datatype of actions. Haskell's do expressions provide a convenient syntax for writing monadic expressions.
> 
> Instances of Monad should satisfy the following:
> 
> Left identity
> 
>     return a >>= k = k a
> 
> Right identity
> 
>     m >>= return = m
> 
> Associativity
> 
>     m >>= (\x -> k x >>= h) = (m >>= k) >>= h
> 
> （中略）
> 
> Minimal complete definition
> 
> - (>>=)
> 
> Methods
> 
> (>>=) :: m a -> (a -> m b) -> m b infixl 1
> 
> - Sequentially compose two actions, passing any value produced by the first as an argument to the second.

`Monad f` な型 `f` では、 `Applicative` で使えた `fmap`・`pure`・`<*>` に加えて、`(>>=) :: f a -> (a -> f b) -> f b` が使えるようになります。また、`Monad` のインスタンスは、以下の則を満たすことを想定されています。
- `pure x >>= f = f x`
- `m >>= pure = m`
- `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`


ところで、`indirectQuince`のような直前の結果にのみ依存するものは、`>>=`を使ってシンプルに書くことができました。しかし、ここで示す`indirectPlus`のような、直前以外の結果にも依存するコードを書こうとするとどうしても不格好になってしまいます。
```haskell
{-
indirectPlus l x は、
  l の x 番目の数字を y とする
  l の y 番目の数字を z とする
  l の y + z 番目の数字を w とする
  ここまで上手くいったら Just w を、さもなくば Nothing を返す
-}

indirectPlus :: [Int] -> Int -> Maybe Int
indirectPlus l x =
  (l !? x) >>= (\y ->
    (l !? y) >>= (\z ->
      (l !? (y + z)) >>= (\w ->
        Just w
      )
    )
  )
```
このような関数を書きやすくするために、Haskellにはdo記法というものが用意されています。これは、async/awaitに類似した糖衣構文です。
```haskell
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
  w <- l !? index
  pure (w ^ 2) -- doの最後の行には、m a の値を起きます

-- 糖衣構文を展開すると、次のようになります：
indirectPlusSquare2 :: [Int] -> Int -> Maybe Int
indirectPlusSquare2 l x =
  (l !? x) >>= (\y ->
    (l !? y) >>= (\z ->
      let index = (y + z) ^ 2 in
        (l !? index) >>= (\w ->
          Just (w ^ 2)
        )
    )
  )
```
このように、do記法を用いれば2つ以上前の結果に依存するようなコードをシンプルに記述することができます。

#### 節末問題
1. `MyList` を `Monad` にしてみましょう
2. 1章の章末問題は `>>=` や `do` を使うとシンプルに書くことができます。やってみましょう
3. 次のような「コマンド」を評価する関数 `evalCommand1`と`evalCommands`を定義しましょう
```haskell
data ComMachine = ComMachine {
    comInput   :: [Int] -- 入力列
  , comOutput  :: [Int] -- 出力列
  , comReg     :: Int   -- 主レジスタ
  , comRegSwap :: Int   -- 予備のレジスタ
  }

data Command =
        ComReadInput      -- 入力列の先頭を取り出し、主レジスタに格納する。入力列の長さは1つ短くなる（入力列が空なら失敗）
      | ComReadOutput Int -- 出力列の指定された位置の値を主レジスタに格納する。出力列の長さは変わらない（出力列の指定された位置が存在しないなら失敗）
      | ComWriteOutput    -- レジスタの値を出力列の末尾に追加する。出力列の長さは1つ長くなる
      | ComBackup         -- 予備のレジスタに主レジスタの値をコピーする
      | ComSwapReg        -- 主レジスタと予備のレジスタの値を入れ替える
      | ComAdd            -- 主レジスタの値を予備のレジスタの値に足し、結果を主レジスタに格納する

initMachine :: [Int] -> ComMachine
initMachine input = ComMachine {
    comInput   = input
  , comOutput  = []
  , comReg     = 0
  , comRegSwap = 0
  }

{-
evalCommand1 :: Command -> ComMachine -> Maybe ComMachine と、
evalCommands :: [Command] -> [Int] -> Maybe [Int] という関数を定義しよう
-}
```

### 2.3 状態付きの計算、Stateモナド
「状態付きの計算」を説明するために、その具体例を見てみましょう。疑似乱数生成器は、乱数を生成するたびに自身の状態を更新します。
```haskell
-- 線形合同法は、一個前に生成した乱数から次の乱数を計算する疑似乱数生成アルゴリズムである
lcgs :: Int -> Int
lcgs x = (48271 * x) `mod` 2147483647
```
複数回乱数を生成するには、その時点での状態を覚えておく必要があります。以下のような「乱数を二つ生成して足す」関数では、返り値としてその時点での状態を返す必要がありそうです。
```haskell
add2Randoms :: Int -> (Int, Int)
add2Randoms seed =
  let
    r1 = lcgs seed
    r2 = lcgs r1
  in
    (seed, r1 + r2)

add2RandomsMinusRandom :: Int -> (Int, Int)
add2RandomsMinusRandom seed =
  let
    (seed', r1) = add2Randoms seed
    r2 = lcgs seed'
  in
    (seed', r1 - r2)

-- add2RandomsMinusRandom の結果を二個足して、偶数かどうか判定する
a2rmTwiceAndCheckIfEven :: Int -> (Int, Bool)
a2rmTwiceAndCheckIfEven seed =
  let
    (seed', r1) = add2RandomsMinusRandom seed
    (seed'', r2) = add2RandomsMinusRandom seed'
  in
    (seed'', r1 + r2 `mod` 2 == 0)
```
`Int -> (Int, a)` という型の関数と、同じような内容のコードがたくさん出てきました。書きやすくするために、補助関数を定義してみます。
```haskell
-- 型に別名をつけて読みやすくしてみる
type WithRng a = Int -> (Int, a)

-- ふつうの値を、乱数生成器を使って何かする関数と同じ型にする
withoutRng :: a -> WithRng a
withoutRng x = \seed -> (seed, x)

chainRandom :: (WithRng a) -> (a -> WithRng b) -> WithRng b
chainRandom gen1 aToGen2 = \seed ->
  let
    (seed', r1) = gen1 seed
    (seed'', r2) = aToGen2 r1 seed'
  in
    (seed'', r2)

genRandom :: WithRng Int
genRandom seed = let r1 = lcgs seed in (r1, r1)

add2Randoms' :: WithRng Int
add2Randoms' = chainRandom gen1 (\r1 -> 
                  chainRandom (\r2 ->
                      withoutRng (r1 + r2)
                    ))

add2RandomsMinusRandom' :: WithRng Int
add2RandomsMinusRandom' = chainRandom add2Randoms' (\r1 ->
                            chainRandom gen1 (\r2 ->
                                withoutRng (r1 - r2)
                              ))
                            
a2rmTwiceAndCheckIfEven' :: WithRng Bool
a2rmTwiceAndCheckIfEven' = chainRandom add2RandomsMinusRandom' (\r1 ->
                              chainRandom add2RandomsMinusRandom' (\r2 ->
                                  withoutRng (r1 + r2 `mod` 2 == 0)
                                ))
```
さて、この`chainRandom :: (WithRng a) -> (a -> WithRng b) -> WithRng b` のシグネチャ、にらめっこしていると `>>= :: m a -> (a -> m b) -> m b` と同じに見えてきませんか……？newtypeで`WithRngM`という新しい型を作って、それを`Monad`にしてみます。
```haskell
newtype WithRngM a = WithRngM { runWithRngM :: Int -> (Int, a) }

-- これらの実装は、それぞれの型クラスの則を満たします。
instance Functor WithRngM where
  fmap f (WithRngM g) = WithRngM (\seed ->
                          let (seed', x) = g seed in
                            (seed', f x))

instance Applicative WithRngM where
  pure x = WithRngM (\seed -> (seed, x))
  WithRngM f <*> WithRngM x = WithRngM (\seed ->
                                let (seed', f') = f seed
                                    (seed'', x') = x seed'
                                in
                                  (seed'', f' x'))

instance Monad WithRngM where
-- だいたい chainRandom とおなじ
WithRngM x >>= f = WithRngM (\seed ->
                      let (seed', x') = x seed
                          WithRngM y = f x'
                      in
                        y seed')
```
Monadにしたので、便利なdo記法が使えるようになります。
```haskell
genRandom' :: WithRngM Int
genRandom' = WithRngM (\seed -> let r1 = lcgs seed in (r1, r1))

add2Randoms'' :: WithRngM Int
add2Randoms'' = do
  r1 <- genRandom'
  r2 <- genRandom'
  pure (r1 + r2)

add2RandomsMinusRandom'' :: WithRngM Int
add2RandomsMinusRandom'' = do
  r1 <- add2Randoms''
  r2 <- genRandom'
  pure (r1 - r2)

a2rmTwiceAndCheckIfEven'' :: WithRngM Bool
a2rmTwiceAndCheckIfEven'' = do
  r1 <- add2RandomsMinusRandom''
  r2 <- add2RandomsMinusRandom''
  pure (r1 + r2 `mod` 2 == 0)
```
さて、先ほどは `newtype WithRngM a = WithRngM { runWithRngM :: Int -> (Int, a) }` と定義しましたが、この便利な構造は乱数生成器以外にも使い回せそうです。Haskellの標準ライブラリの`Control.Monad.State`には、`WithRngM` の定義中の `Int` を型変数 `s` に置き換えた、`State s` という型が用意されており、これは先ほどの`WithRngM`と同様に使うことができます。
```haskell
-- ファイルの先頭に import Control.Monad.State を書いておく

-- Stateの定義
-- newtype State s a = State { runState :: s -> (s, a) }

genRandom'' :: State Int Int
genRandom'' = State (\seed -> let r1 = lcgs seed in (r1, r1))

add2Randoms''' :: State Int Int
add2Randoms''' = do
  r1 <- genRandom''
  r2 <- genRandom''
  pure (r1 + r2)

add2RandomsMinusRandom''' :: State Int Int
add2RandomsMinusRandom''' = do
  r1 <- add2Randoms'''
  r2 <- genRandom''
  pure (r1 - r2)

a2rmTwiceAndCheckIfEven''' :: State Int Bool
a2rmTwiceAndCheckIfEven''' = do
  r1 <- add2RandomsMinusRandom'''
  r2 <- add2RandomsMinusRandom'''
  pure (r1 + r2 `mod` 2 == 0)
```
また、状態の変更をしない場合には `State` の亜種である `Reader` を使えばそれを明示することができます（`Control.Monad.Reader`内で定義されています）。
```haskell
-- newtype Reader r a = Reader { runReader :: r -> a }
```
#### 節末問題
1. ターン制バトルを実装してみましょう。
```haskell
data BattlePlayer = BattlePlayer {
    plName :: String
  , plHp   :: Int
  , plMp   :: Int
  , plAtk  :: Int
  , plDef  :: Int
  }

data BattleSpell =
    Fireball String
  | Heal     String
  | Attack   String
  | Charge   String





```

### 2.6 IOと向き合う
第0章で、「副作用」はIOアクションと呼ばれる特殊な値として扱われるという話をしました。このIOアクションは、「外界とあれやこれやして型aの値を手に入れる手続き」という意味の`IO a` という型で表されます。

Preludeは、`getLine :: IO String` や `putStrLn :: String -> IO ()` などのいろいろなIOアクションを提供しています。ghci上でIOアクションを評価すると、そのアクションが実行されます。試してみましょう。
```
ghci> putStrLn "(^^)" 
(^^)
```

Haskellの標準ライブラリでは、IOアクションどうしをつなげる関数、具体的には、`action1 :: IO a` とその結果に依存する次のアクション `resToAction2 :: a -> IO b` を立て続けに行う新しいアクションを作る関数が用意されています。

なにを隠そう、その関数は本章でさんざん出てきた `>>= :: m a -> (a -> m b) -> m b` なのです。`IO` は、上で述べたような「アクションをつなげる関数」を`>>=`の実装として採用すると`Monad`になります。（`>>=`の具体的な実装について述べようとすると`IO`の正体に言及することになりますが、それは面倒なのでここでは避けます。）

`IO` の `>>=` を実際に使ってみましょう。たとえば、`getLine :: IO String` で受け取った入力を `putStrLn :: String -> IO ()` で出力するアクションは、`getLine >>= putStrLn` と書くことができます。
```
ghci> getLine >>= putStrLn
やっほー！
やっほー！
```
`IO` も`Monad`の一種ですから、便利なdo記法を使うことができます。
```haskell
concat2Inputs :: IO ()
concat2Inputs = do
  x <- getLine
  y <- getLine
  putStrLn (x ++ y)
```
Haskellの処理系は、`main` と名のついた型`IO ()`のIOアクションをエントリポイントとして扱います。新しいファイル `nyuumon2.hs` を作製し、以下のように書いてみましょう。
```haskell
main :: IO ()
main = do
  putStrLn "Hello, world!"
```
このファイルをコンパイルして実行してみましょう。
```
$ ghc nyuumon2.hs
$ ./nyuumon2
Hello, world!
```
いままでの章で紹介したような関数をIOアクションにつなげれば、任意のプログラムを書くことができます。「とりあえずHaskellで動くものを書きたい」というだけであれば本節の内容まででもなんとかなりますが、実用の章である第3章の前に、モナド変換子という非常に便利な型に触れることとします。

### 2.6 モナド変換子でモナドを改造する

