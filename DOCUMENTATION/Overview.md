Cookbook Library
=================
This is the Cookbook library overview file. Cookbook has become a beast of a complex hierarchy of related libraries, and this file will explain the purpose of each individual library and file, but not function. Functions in Cookbook are pretty volatile, and can change at any point. Consult the Haddock documentation, Call Trace, or dependency chart for information regarding functions.

Cookbook.Essential
-------------------
Essential is the root of all Cookbook functions. Essential defines widely used interfaces for manipulating lists and communicating with the outside world. Add your code to Essential if the core libraries in Ingredients rely on it heavily, or if it doesn't fit anywhere else (especially if it doesn't rely on any Cookbook functions). 

It can be debated that Cookbook.Essential.Continuous is out of place in Cookbook.Essential because it relies on Cm, Br, and Ac, but it will remain in Essential for the time being. Cookbook was designed around Continuous (check the git history), and it will remain such a core part of the library.

### Common.hs
Cookbook.Essential.Common (Cm) is the true root of Cookbook. It relies on nothing, and takes no prisoners. It defines functions that lay the framework for the rest of the Cookbook library, such as sub and positions. In Cookbook libraries, the Common library is there to "pad" the features of a language to make implementing the rest of Cookbook possible.

### Continuous.hs
Cookbook.Essential.Continuous (Ct) is something called an "Overloaded generic interface" in Cookbook. An OGI defines a set of behavior, but not what kind of data it works on. This is a bit of a hack, because anything works inside of the function. You do not have to constrain against Cnt.Continuous() even though the typeclass implements the behavior of the library. Long story short: be careful with what kind of Data you use after, before, and delete on.

### IO.hs
Cookbook.Essential.IO (Co) is a relatively new and unused library. It performs actions with IO, and currently only has two. IO actions in Haskell are usually VERY domain-specific, so not much ever makes it into CIO. However, what does work works well, and is usually safer than trying to implement the IO yourself. CIO accounts for laziness, while not jumblings things up with libraries like Bytestring.

Ingredients
===============
If the root of Cookbook (Cookbook.Essential) is the stove, Ingredients are the stew. Ingredients is where most of the generic defintions are. Ingredients is grouped up by category based on the type of data it handles, and subdivided into Modules based on what kind of actions are being performed on that data.

Functional
------------
Functional is the library for composing functions which return functions or take functions as an argument. Other libraries are allowed to take functions are parameters and return them, but Functional is for libraries for the dedicated purpose of serving the needs of higher-orderism.

### Break.hs
Cookbook.Ingredients.Functional.Breaks (Br): So far, the only library is Breaks. Breaks are like normal maps / filters, except they will stop execution at the first untrue value.

Lists
--------
Lists is the most widely used library in Cookbook. Lists groups operatings on generic lists, including replacing parts of them, modifying their contents (immutably, of course. No IO here), accessing their contents, and running statistical tests on them. If a function is defined in terms of a list, and it doesn't belong in Functional or tupples, it is usually welcomed in the list library.

### Access.hs
Cookbook.Ingredients.Lists.Access (Ac) is a library for accessing the contents of lists and performing some tests on them. It will return a modified version of the list in some cases, but the sole purpose is to learn more information about what is inside of the list, and to group it accordingly. 

### Modify.hs
Cookbook.Ingredients.Lists.Modify (Md) is a library for modifying the contents of lists. Reversing is an example of a modification function, beacuse it doesn't tell us any other information about the list other than what its contents can already tell us. If the modification of a list is the result of an algorithm (as is the case with qsort), then it should be placed in Access or Stats, because they are designed for List tests and Frequency Analysis.

### Replace.hs
Cookbook.Ingredients.Lists.Replace (Rp) is a library for replacing parts of lists. Rp is a OGI like Continuous is, working on every permutation of possible arguments to the function. 

### Stats.hs
Cookbook.Ingredients.Lists.Stats (St) is a library for Statistical analysis on lists. Currently the library has a bunch of code for frequency analysis in there. Any type of statistical test (or test out of place in the other libraries) can be put in Stats.hs

### Remove.hs
Cookbook.Ingredients.Lists.Remove (Rm) is a library for removing items from lists. Like a lot of important Cookbook functions, Rm is an OGI overloaded for lists and single items. It also supports a tupple of two items, where it will remove the text between them, and the text in the tupples.
Tupples
--------
Tupples are a curious case of Haskell data. They are restricted to a length, which can be a pain when defining a library meant to work on Generics. Cookbook.Ingredients.Tupples is meant to work on two-sided tupples, because they are the most common use of tupples. (See: Zip)

### Assemble.hs
Cookbook.Ingredients.Tupples.Assemble (As) is a library for assembling tupples. The assembly process involves ordering a list of tupples which have a member of the Ord typeclass in their eastern slot (snd), and then mapping fst over them.

### Look.hs
Cookbook.Ingredients.Tupples.Look (Lk) is a library for finding tupples in a list. Like associative lists in Common Lisp, Look (the function defined in these files) and looklist (gets all of the tupples, unsafely) will search for an exact match in the fst, and then return the snd when it finds it.

Recipes
========
What is a chef without some delicious recipes? A skilled cullinary artisan should be able to make themselves a meal without direction, but what about when the guest he's serving asks for an Exotic meadly of all sorts of junk you've never heard of? This is where Recipes comes in. Recipes either do not work on Generics fully, or are domain-specific while still working with generics.

### Detect.hs
Cookbook.Recipes.Detect (Dt) is a library for using generic symbols for text. It has a generic underpinning, which functions interact with to make a defined standard for a "strpex". A strpex is a way of defining the "shape" of text, so that algorithms inside of Dt can search for them.

### DiffStat.hs
Cookbook.Recipes.DiffStat (Ds) is a library for finding the differences between lists, and recreating the changes that caused the difference between the two original changes. If you are familiar with diff and patch on Unix systems, Ds solves the same problem.

### Sanitize.hs
Cookbook.Recipes.Sanitize (Sn) is a library for performing common sanitization techniques on text and lists. It can do things like remove doubles from Lists, or move characters to uppercase.

### DataStructures.hs
Cookbook.Recipes.DataStructures (Dt) is a library for organizing ideas into higher-level data. Data structures specific to one use (such as the Databases from Sc) should be kept to the Project branch. This is for generic data structures, such as Tree. For functions working with these data structures, use Ag.

### Math.hs
Cookbook.Recipes.Math (Mt) is a library for performing simple mathematical functions and statistical analysis. Simple functions like inc and dec ((+1) (-1)) are included for convenience, and more complex formulae (standard deviation) are included as well.

### Algorithm.hs
Cookbook.Recipes.Algorithm (Ag) is a library for parsing, manipulating, and using Dt's data structures in any fashion.

Projects
==========
Projects is the final branch of the Cookbook library. Projects is a defintion of standards and an implementation of this defintiion. This means that it can contain "magic characters" which are specific to the set of standards defined. Items in Projects assume a certain structure that make them void to use anywhere else. The exact structure should be defined in a file next to the library with the ".std" extension. By convention, all Project files are inside themselves.

### Configuration.hs
Cookbook.Projects.Configuration.Configuration (Cf) is a library for reading the most simple type of configuration file available. It expects that the files have lines seperated by commas, where the left is a configuration option, and the right is its value.

### Groups.hs
Cookbook.Projects.Groups.Groups (Gp) is a group library. It defines a markup language that uses "object-orientation" to define how data should look, and then allows user implementations of such data.

### Scribe.hs
Scribe is a language for use with databases. Like SQL databases, Scribe supports tables, rows, columns, and individual entries. Scribe is "self-aware" in the sense that it can reference its own tables through a function called "Promises". When a Promise is made, it is left  unparsed as a Frame type (construct Promise {keyval=name,link=otherTable}). There is no implementation to resolve promises. This is left up to the discretion of the application using Scribe.

### Preprocess.hs
Preprocess is a library for reading the preprocess language. Preprocess maps multiple inputs to one output for use in chat bots and text preprocessors. Preprocess supports comments between two dollar signs $$. An odd number of dollar signs can be used for use in mapping either inputs or outputs. Inline comments will be removed as well.

### Quill.hs
Quill is a library for reading and returning very simple read-only databases. It currently does not selectively parse like Scribe does, and is a non-whitespace significant language, unlike Scribe. Comment support will come at some point in time, as well as a richer API than the tables function.
