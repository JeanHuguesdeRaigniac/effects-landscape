# All Cats Are Grey

In the [previous article](../v0/README.md), we described how to use 10 effect libraries to run a sample application with common effects: writer, state, reader and error. By comparing them, we were able to highlight a pattern in their usage. Could we standardize it further and erase library details? Yes!

Today, we will

- hide library implementations under a common abstract signature,
- enrich our effects' collection with one of our make and
- give a new interpretation to our application with tests, using `Identity` as base monad.

Some plumbing is needed to test each library against our new generic code from the command line. It is also explained for completeness.

End notes describe what remains to do.

## How to box a library

To unite our effect libraries under the same signature, we have to extract their Least Common Denominator.

Effects may have specific names in libraries. For example, `extensible-effects` calls `Exc` the one dealing with errors. So we need a unique set of types: `Error`, `Reader`, `State` and `Writer`. To these we will add `Log`, the new effect described below.

Then, we saw that `tick` and `eval` function [signatures](../v0/README.md#signatures) have a similar shape of 3 elements. One or more constraints, optional parameters, and an effect monad wrapping a value.

Parameters are not relevant to our goal, so let's start shoehorning constraints into a common model.

### Constraints

#### Tell me your type...

In the context of effects systems, we may describe a single constraint as "this effect I need exists in the effect monad". Its type is:

```Haskell
C e sig es
```

`e` is our effect, `es` the type holding all effects. You may ignore `sig`, it is only needed for `fused-effects`.

For many constraints, it simply translates to "each of these effects I need exists in the effect monad":

```Haskell
CS es sig m
```

`es` is the effects we need and `m`, as before, the full stack of effects available (same remark for `sig`).

#### ... and how to reify it with a library

We just have to resolve constraints according to each library's way.

For `C`, it could be the simple way of `transformers`:

```Haskell
type C e sig es = (Monad es)
```

But sometimes a type family is needed to map our common effect type to the one used in a given library, like for `has-transformers`:

```Haskell
type C e sig es = (Monad es, Reify e es)

type family Reify e es where
  Reify Log m = HasLog m
  Reify (Error t) m = HasExcept t m
  Reify (Reader t) m = HasReader t m
  Reify (State t) m = HasState t m
  Reify (Writer t) m = HasWriter t m
```

`CS` always needs a type family to recursively resolve each constraint:

```Haskell
type family Constrain es :: Constraint where
  Constrain '[] = ()
  Constrain (c ': es) = (c, Constrain es)
```

Its call is often of the form:

```Haskell
type CS es sig m = Constrain es m
```

Constraints resolution and type family definitions are tailored for each library, so you will find many variations in modules.

### Effect monads

The second element of a common signature is the effect monad (here with its translation for `mtl`):

```Haskell
type ES m a = m a
```

`ES` is the EffectS type, holding a monad `m` and a value `a`. It could be written `type ES effects a` as well, to highlight the fact it stores all effects declared in our application.

You will find code for each library module in [src/Internal/](../src/Internal/) ("shoehorning" section.)

### Resulting signatures

We are now able to use these new generic signatures in [App.hs](../v1/App.hs):

```Haskell
tick :: C (State Steps) sig m => ES m ()
...

eval ::
  CS
    '[ C Log sig m,
       C (Error String) sig m,
       C (Reader Env) sig m,
       C (State Steps) sig m,
       C (Writer Variables) sig m
     ]
    sig
    m =>
  Exp ->
  ES m Value
...
```

By abstracting these constraints and types, we remove the [previous version](../v0/) code duplication of `App<library>.hs` modules.  There is only one declaration in [App.hs](../v1/App.hs), and we switch library implementation on the command line. We get a _de facto_ common usage for all libraries!

Up to now, we have used already defined effects, it is time to make one.

## How do you say?

Before, `embeddedLog` was implemented as a direct call to Prelude's function `putStrLn`, or delegated to a library `Trace` effect. It is a good candidate for the job.

The usual way to define an effect is to use a GADT and Template Haskell to generate functions. Here is how it looks like with `polysemy` (at the same time, `embeddedLog` is renamed as `log`):

```Haskell
data Log m a where
  Log :: Text -> Log m ()

makeSem ''Log
```

Compared to our previous hard coded version, the advantage is that `m` is not tied anymore with `IO`. It allows different interpretations, as we will see in the next section.

For most libraries, defining a new effect is as easy as the example above (look for the "effect" section in modules). Ahem, `fused-effects` is clearly not in that category. `mtl` illustrates the now famous n² instances problem. Designed for reuse, `rio` is quite easy to adapt since the code to add is similar to the one of other "effects", but it requires a first exposition to its logging system. `transformers` is the only library with a direct `MonadIO` constraint.

We interpret this effect with the `runLog` function, it is where the dependency on `IO` appears ("interpreters" section). While usually short, code is not always obvious or uniform.

With this new feature, it is easier to refactor tests to run them on top of the `Identity` monad.

## Pure tests

The function to execute our application, `runEval`, has a different signature in the [AppSpec.hs](../test/AppSpec.hs) module than the one in [App.hs](../v1/App.hs): its return value is not wrapped in a monad, and we add logs to the resulting tuple.

```Haskell
-- v1/App.hs
runEval :: Env -> Steps -> Exp -> IO (Either String Value, Steps, Variables)

-- test/AppSpec.hs
runEval :: Env -> Steps -> Exp -> (Either String Value, Steps, Variables, Logs)
```

`Identity` could have been used to replace `IO`, signatures would share the same shape. In the end, both phrasing show we have a pure computation.

The `run` function discards the carrying monad, its signature is often:

```Haskell
run :: Eff '[] a -> a

-- or

run :: Identity a -> a
```

Since `rio` means reader over `IO`, we can't replace it with `Identity`. We are then entitled to use, for demonstration purpose and at the risk of eternal curse, THE FUNCTION WHICH CAN'T BE NAMED:

```Haskell
run :: IO a -> a
run = unsafePerformIO
```

The other notable differences are the addition of the `runLogWriter` function and the renaming of `runWriter` to `runVariableWriter`.

The first one is obvious: in this new interpretation we don't print messages, we collect them in a list with a `Writer`. But why the second?

Last time, we have seen that GHC inference struggles at times with effect systems. `Variables` and `Logs` are managed by the same effect, `Writer`. Some may have noticed the use of `Text` as parameter to the `log` function, and not `String`, like with `embeddedLog`. The reason is simple, we would have ended with two identical `Writer [String]` in our stack. The compiler can't know when to use which one, end of story.

Since this repository's purpose is to document effect systems, there is no reason to look for an elaborate solution: by using `Text`, we set aside our 2 writers on their type parameter, and to help inference, we use type application in a dedicated function, `runVariableWriter`. Production code would probably distinguish both types by wrapping them in newtypes. [`effet`](https://hackage.haskell.org/package/effet) offers another interesting mechanism to disambiguate effects of the same type. Unfortunately this library needs an update to compile with `GHC` 9.2.7, the version used while preparing this post.

You will find the same sections in [test/Internal/](../test/Internal/) modules ("shoehorning", "effect", when needed, and "interpreters".)

Like with the "production" version running in `IO`, tests use only one definition of `tick`, `eval` and `runEval` in the [AppSpec.hs](../test/AppSpec.hs) module. Accordingly, we also switch library implementation on the command line.

This ends our explanation of generic signatures, custom effect and pure interpretation. But we have not yet seen how to run this school of interpreters!

## Cabal's arcana

To use the same code with different libraries, we need a way to "inject" a different implementation into our application. It's, in fact, more cumbersome than difficult.

We declare flags in [effects-landscape.cabal](../effects-landscape.cabal), for example `flag with-cleff`, and when one is used on the command line, we map it to a CPP option:

```cabal
    if flag(with-cleff)
        cpp-options: -DWITH_CLEFF
```

Then, in [App.hs](../v1/App.hs) for instance, the CPP language pragma makes it possible to compile a module with the specified library:

```Haskell
#ifdef WITH_CLEFF
import Internal.WithCleff
#endif
```

Since all flags are `False` by default, use one to expect a result:

```bash
# IO interpreter
cabal -fwith-cleff run v1

# pure interpreter
cabal -fwith-cleff test
```

Let's face it. It's ugly, but it does the job.

## What next?

The clear goal of this article and the previous one is to compare effects libraries by using them with the same application. Last time, we normalized usage. Today, we uncovered a common structure, made a custom effect and tested our application with other interpreters.

It certainly helps to assess their ease of use, but the current set of features is not representative enough. For example, how to use functions like [`forConcurrently`](https://hackage.haskell.org/package/async-2.2.4/docs/Control-Concurrent-Async.html#v:forConcurrently) or [`bracket`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Exception.html#v:bracket). The presence of a monad in their signature requires "higher-order" effects.

Another common use case is to combine an external monad stack with ours, Servant's [`Handler`](https://hackage.haskell.org/package/servant-server-0.19.2/docs/Servant-Server.html#t:Handler) or Persistent's [SqlPersistM](https://hackage.haskell.org/package/persistent-2.14.5.0/docs/Database-Persist-Sql.html#t:SqlPersistM) for instance.

The current test suite is also incomplete. Collecting logs in a list, while useful, is not very flexible or rich. Say you have this effect:

```Haskell
data Foo m a where
  Bar :: Int -> Foo m ()
  Baz :: Double -> Double -> Foo m Double
```

Getting as output a list of operations

```Haskell
[Op (Bar 42), Op (Bar 84), Op (Baz 10 32)]
```

Would be much more interesting to check than a list of Strings

```Haskell
["Bar 42", "Bar 84", "Baz 10 32"]
```

All these examples are still related to library usage. What about runtime behavior? We know that micro-benchmarks are not accurate. Yet, if they were written in the first place, it is because we need to check memory usage or performance. This is not something a strong type system or equational reasoning give us.

Using sample Applications from [Implementing Clean Architecture with Haskell and Polysemy](https://github.com/thma/PolysemyCleanArchitecture) or [MSc Dissertation: Comprehending Pure Functional Effect Systems](https://www.dantb.dev/posts/dissertation/) with "real life" data volume would give a satisfying approximation (by the way, I find these works very good).

You get it, we have not yet reached our destination.

## Post Scriptum

I take this opportunity to say I am looking for another Haskell job. Working at [Decathlon](https://www.decathlon.com/) is pleasant, my team nice. But the Haskell part is almost over, and having to switch to Java seems insipid in comparison. That is for sure a side effect of Haskell, it spoils you!

This [video](https://youtu.be/rf-eOeTcnCs) (15s) and this [one](https://youtu.be/Vq6mGeCkogw) (1:42s) show that I love working on development tooling and diving into compilers bowels. Here is my [LinkedIn](https://www.linkedin.com/in/jean-hugues-de-raigniac/?locale=en_US) profile.
