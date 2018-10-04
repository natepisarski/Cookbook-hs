Introduction
--------------
Cookbook is a general-purpose Haskell library. It is in a way like boost, but without all the things that makes boost bad (of which there are plenty).

The library builds upon itself to create a complex, but independant cluster of low-dependency code with widely applicable reuse possibilities. To develop Cookbook, I add functions to it whenever I need them in another unrelated project. Because of this, Cookbook slowly grows to an all-encompassing behemoth.

Organization
-------------
The library is a bit split up. It has four "Wings": Essential, Ingredients, Recipes, and Projects.

### Essential
Essential is where the most general functions of the libraries go. It's also where ALL continuations are placed. The continuation is an important concept within Cookbook, and a lot of Code relies on it. It's basically a high level of polymorphism (think: Operator Overloading) supported by GHC's MultiParameter Typeclasses. In layman's terms, (after "tes.t" '.') and (after "tes.t" "s.") will yield the same result.

### Ingredients
This is the primary branch of Cookbook, although it is far from being the largest. It is subdivided into different essential types of data in Haskell (Lists, Tupples, Functions), and further subdivided into uses for each.

### Recipes
The Recipes branch is for operating on specific types of data, like Strings or Ints, but they are generally applicable. This means that there is no format the data should be required to be in, just the data itself needs to be present.

### Project
This is the least general of the branches in Cookbook. The Project branch contains experimental (and somewhat dangerous) code that is described in terms of a "standard" (.std file). It expects a certain format, and in a lot of cases will bomb out if it's NOT given that format (think: Database files).

Overview
------------

This is the general overview for all files in Cookbook. For a more in-depth look at each and what it does, take a gander into the DOCUMENTATION branch. To the right of each file is the file's short name.

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
Cookbook can be installed on hackage with the command "cabal install cookbook". You can also install the package resulting from "runhaskell Setup.hs configure && runhaskell Setup.hs build" with cabal if that's your thing. There is now a Debian package available, but it will literally just run "cabal install cookbook", so get it from the source to stay up to date.

If you do not want to install Cookbook in a global position, place the Cookbook folder in your project's root directory. Because libraries use an unqualified header-level name, they can be called this way through "Cookbook.*" like normal.

Links
-------
[hackage](http://hackage.haskell.org/package/cookbook)
