{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:numis' --allow-eval --warnings";
  testScript = s "tst" "cabal run test:numis-tests";
  hoogleScript = s "hgl" "hoogle serve";
}
