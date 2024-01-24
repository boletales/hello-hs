
### 処理系との約束事
- みなさんも、他の言語で「mainから実行される」みたいな約束事を見たことがあるとおもう
1. プログラム中のすべての式は、それを評価した値に書き換えても（計算時間をのぞいて）プログラムの振る舞いを変えない
   - 端的にいえば、「関数はすべて純粋です」ということ
   - 例えば、`someFunc 6` が `36` になるならば、プログラム中のすべての `someFunc 6` を `36` に置き換えてもプログラムの振る舞いは変わらない
   - コンパイラにとってこの約束は「返り値さえ同じなら、めっちゃアグレッシブに最適化をしてもかまわない」ということ
2. この約束はややこしいので、しばらく忘れていてほしい→ ~~プログラムが実行される際には、`main`という名で型が`IO ()`の手続きが走る~~
   - javascript風に説明するならば、プログラマは`main`という名のクソデカPromiseを組み立てて、実行時にはそれが走るということ