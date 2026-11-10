# tapl-hs

TAPL (型システム入門) の内容を Haskell で実装するプロジェクト。

## テスト

すべてのテストを実行する:

```bash
cabal test
```

モジュールごとに実行するには `--match` にモジュール名を渡す:

```bash
cabal test --test-options='--match "/SExpr/"'
```

```bash
cabal test --test-options='--match "/Arith.Syntax/"'
```

```bash
cabal test --test-options='--match "/Arith.Eval/"'
```

```bash
cabal test --test-options='--match "/Lambda.Lambda/"'
```

関数単位まで絞ることもできる (`describe` の名前を `/` でつなぐ):

```bash
cabal test --test-options='--match "/Lambda.Lambda/termShift/"'
```

テストは `test/` 以下の `*Spec.hs` に置く。hspec-discover が自動で拾うので、
追加したら `tapl-hs.cabal` の `other-modules` にモジュール名を足す。
