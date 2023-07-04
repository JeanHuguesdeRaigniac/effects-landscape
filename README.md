# Posts

[One Step Beyond](v0/README.md) (January 9th 2023)

[All Cats Are Grey](v1/README.md) (May 22nd 2023)

In the second post, without a library flag set to `True` in the [effects-landscape.cabal](effects-landscape.cabal) file, HLS does not work (signature on hovering, going to definition...).

Now, the first library is selected by default. To use another one, set its flag to `True` and the previous selected one to `False`.

For example, to use Effectful, do:

```cabal
flag with-cleff
    default: False
flag with-effectful
    default: True
```

And restart Haskell LSP server.
