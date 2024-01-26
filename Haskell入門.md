# Haskell入門

## 第0章 はじめに
### 0.1 モチベーション
このドキュメントでは、Haskellの環境構築と基本的な文法を学んだのち、パーサコンビネータライブラリを用いた程度実用的なプログラムを書くことを目標とします。読者の想定として、いままでに2つ以上のプログラミング言語を学んだことのある人を想定しています。

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

これらの特徴がうまくかみ合うような場面では、Haskellを使うと高い生産性を実現できます。コンパイラや自作言語の実装においてHaskellの右に出る言語はおそらくないですし、Webサーバーのような副作用の流れが複雑な場面でも優秀です。上の特徴にがっつりかみ合う場面でなくても、普段使い用のまともに型が付いてスクリプトっぽい書き味が実現できる（実際に、GHCのインタプリタモードを使えばスクリプトとして実行できます）言語、つまるところPythonしか知らない人のPython代替としてはそれなりの使いやすさがあります。

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

### 1.4 自家製の型・できあいの型・型変数・カインド
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
  - 「型である」「型変数を一つ受け取ると具体的な型になる」のような属性のことを、Kindと呼ぶ
  - ghciで `:k List` と打つと、`List :: * -> *` と表示される
    - `*` は具体的な型を表すKindである
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
- `List a` の各要素に型 `a -> b` の関数を適用する関数 `lmap :: (a -> b) -> List a -> List b` を定義してみよう

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
  