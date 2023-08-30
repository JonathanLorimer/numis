{s}: 
{
  devLibScript = s "dev" "ghcid --command 'cabal new-repl lib:numis' --allow-eval --warnings";
  devCliScript = s "dev:cli" "ghcid --command 'cabal new-repl lib:cli' --allow-eval --warnings";
  testScript = s "tst" "cabal run test:numis-tests";
  hoogleScript = s "hgl" "hoogle serve";
}
