# release-bot

## Nix support

If `package.yaml` has been updated, one should refresh the nix file:

```
cabal2nix --hpack . > release-bot.nix
```

You can then build it:

```
nix-build
```
