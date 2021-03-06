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
Continuous is an important Coobkook library for interacting with variable kinds of data (Ct). It is able to adapt to its input, acting as overloaded functions do in other languages.

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

### Stats.hs
Cookbook.Ingredients.Lists.Stats (St) is a library for Statistical analysis on lists. Currently the library has a bunch of code for frequency analysis in there. Any type of statistical test (or test out of place in the other libraries) can be put in Stats.hs

### Encompass.hs
Cookbook.Ingredients.Lists.Encompass (En) is a library for lexical analyzation. It has the ability to split up strings based on scope, and selectively parse.

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

### Sanitize.hs
Cookbook.Recipes.Sanitize (Sn) is a library for performing common sanitization techniques on text and lists. It can do things like remove doubles from Lists, or move characters to uppercase.

### DataStructures.hs
Cookbook.Recipes.DataStructures (Dt) is a library for organizing ideas into higher-level data. Data structures specific to one use (such as the Databases from Sc) should be kept to the Project branch. This is for generic data structures, such as Tree. For functions working with these data structures, use Ag.

### Math.hs
Cookbook.Recipes.Math (Mt) is a library for performing simple mathematical functions and statistical analysis. Simple functions like inc and dec ((+1) (-1)) are included for convenience, and more complex formulae (standard deviation) are included as well.

### Cline.hs
Cookbook.Recipes.Cline (Cl) is a library for parsing command-line arguments. It currently supports flags and parameterized arguments.

### Algorithm.hs
Cookbook.Recipes.Algorithm (Ag) is a library for parsing, manipulating, and using Dt's data structures in any fashion.

### WordStats.hs
Cookbook.Recipes.WordStats (Ws) is a library for determining the probabibility that two words are the same. Loosely ported from concepts in the tier-1 version of Cookbook (called LibHaskell), it analyzes the frequency of letters and the length of the string to determine an object's 
Projects
==========
Projects is the final branch of the Cookbook library. Projects is a defintion of standards and an implementation of this defintiion. This means that it can contain "magic characters" which are specific to the set of standards defined. Items in Projects assume a certain structure that make them void to use anywhere else. The exact structure should be defined in a file next to the library with the ".std" extension. By convention, all Project files are inside themselves.

### Configuration.hs
Cookbook.Projects.Configuration.Configuration (Cf) is a library for reading the most simple type of configuration file available. It expects that the files have lines seperated by commas, where the left is a configuration option, and the right is its value.

### Preprocess.hs
Preprocess is a library for reading the preprocess language. Preprocess maps multiple inputs to one output for use in chat bots and text preprocessors. Preprocess supports comments between two dollar signs $$. An odd number of dollar signs can be used for use in mapping either inputs or outputs. Inline comments will be removed as well.

### Quill2

     Q2Prelude.hs
Q2Prelude is the entry-level module for Quill2. It defines the data types that define data, databases, and errors throughout the rest of Quill2.

     Q2Parse.hs
Q2Parse is the "engine" of Quill2. It is able to turn a string (or a list of them) into a Quill database. The syntax of Quill currently supports Whitespace-inclusive strings, comments, and whitespace-independence.

     Q2Api.hs
Q2Api is the user-facing part of Quill. It has all the functions necessar to Create, Read, Update, and Delete information from the database, and turn a database back into a Quill-readable string.

     Q2Io.hs
Q2Io is a helper library for interacting with Quill files. It aids in the reading of and writing to files on the user's system.
