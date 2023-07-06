Introduction
--------------
Cookbook is a general-purpose Haskell library, and seminal pet project of mine. It has no external dependencies, but most modules are "inter-dependent" with another one within Cookbook.

The library was developed with a "pragmatic philosophy": for 3 years, if I needed some function for a different project, I just added it here. This led to a fragmented but versatile package.


Organization
-------------
Cookbook has 4 "Wings": Essential, Ingredients, Recipes, and Projects.

### Essential
Essential is where the most general functions of the libraries go. It's also where ALL **continuations** are placed. A **continuation** in Cookbook can be thought of like a strong method overload / dynamic dispatch. In practice, these are just functions which use GHC's MultiParameter Typeclasses.

For instance, these 2 lines yield the same result:

```haskell
after "tes.t" '.'
```

```haskell
after "test.t" "s."
```

### Ingredients
The primary wing of cookbook, for general Haskell utilities. It is subdivided into Haskell primitives (`Lists`, `Tuples`, `Functions`), which and further subdivided into specific use-cases.

### Recipes
The wing that requires specific data types, but is still generally applicable. For instance, you might find a function called `reverseString` in here, but not `parseJson` because JSON is not a "general" technology.

### Project
The least general Wing. The `Project` branch contains experimental code that is described in terms of a "standard" (.std file). It expects a certain format, and in a lot of cases will bomb out if it's NOT given that format (think: Database files).

Overview
------------

This is the "minimap" of Cookbook. Here you can see the overall structure, as well as the **short-name** of each file. For more information, consult the `DOCUMENTATION`directory.

> **Note:** A file's "short-name" is the convention used during an import

* Essential
  * Common.hs     (Cm)
  * Continuous.hs (Ct)
  * IO.hs         (Io)
* Ingredients
  * Functional
    * Break.hs (Br)
  * Lists
    * Access.hs    (Ac)
    * Encompass.hs (En)
    * Modify.hs    (Md)
    * Stats.hs     (St)
  * Tupples
    * Assemble.hs (As)
    * Look.hs     (Lk)
* Recipes
  * Algorithm.hs      (Ag)
  * Cline.hs          (Cl)
  * DataStructures.hs (Ds)
  * Math.hs           (Ma)
  * Sanitize.hs       (Sn)
  * WordStats.hs      (Ws)
* Project
  * Configuration
    * Configuration.hs (Cf)
    * Configuration.std
  * Preprocess
    * Preprocess.hs (Pp)
    * Preprocess.std
  * Quill2
      * Q2Prelude (Q2p)
      * Q2Parse   (Q2r)
      * Q2Api     (Q2A)
      * Q2Io      (Q2Io)
      * Q2.std
    * Quill.std
    
Installing
-------------
Cookbook is on hackage, so you can use

```shell
cabal install cookbook
```

If you do not want to install Cookbook in a global position, place the Cookbook folder in your project's root directory. Because libraries use an unqualified header-level name, they can be called this way through "Cookbook.*" like normal.

Links
-------
[hackage](http://hackage.haskell.org/package/cookbook)
