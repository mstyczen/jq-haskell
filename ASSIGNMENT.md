# Implementing `jq` in Haskell

`jq` [(stedolan.github.io/jq)](https://stedolan.github.io/jq/) is a JSON processor.
It's built in the spirit of Unix: doing one thing, but doing it well.
Think `awk`, `sed` or `grep`, but for JSON.
And we're going to build its clone, but in Haskell!

In this text, we provide you with a description of what we expect you to
implement. **Please read it fully and carefully.** The assignment is divided
into two parts: a basic part and a set of optional extensions. This means that
you are not required to execute them to get a passing grade on the project.
Implementing the optional assignments correctly will lead to higher grades, but
only if your implementation of the basic part is of acceptable quality.

This is an *individual project*, which means you should not collaborate
directly with other students. However, you are allowed to discuss the
assignment with other students and ask general questions on Stack Overflow
Delft. **Under no circumstances should you ever look at complete or partial
solutions by another student, or show (part of) your code to another student.**
See [www.tudelft.nl/en/student/legal-position/fraud-plagiarism/](https://www.tudelft.nl/en/student/legal-position/fraud-plagiarism/)
for the general TU Delft policy regarding fraud.

It is expected that completing the project will take you approximately 30-40
hours. When you have finished the project, you should hand in your final
solution via Weblab before **8 April 2021 at 23:59**. Detailed instructions for
submitting your solution will appear on Weblab some weeks before the deadline.

## Introducing `jq`

First let's see what you can actually do in it.

### Case study: counting meteorites

Let's find out how many [meteorites](https://xkcd.com/1723/) fell in the Netherlands!

<details>
<summary><b>Alternative ways to follow this tutorial (Windows, web)</b></summary>
If you're on Windows, [here's](https://gitlab.tudelft.nl/bliesnikov/jq-clone/-/snippets/171) a version for PowerShell.


While the best way to follow this part of the intro is with a shell open, we also put the data on jqplay.org for you to play with.
It's not quite as fast as your local installation, but works in your favourite browser.  
Runs slower: [full dataset](https://jqplay.org/s/McozX7_-j-).  
Runs faster: (incomplete dataset) [top 100 heaviest meteorites](https://jqplay.org/s/wz6ZT0S5ky).
</details>

NASA provides a database of meteorites, as JSON object, so let's download it:
    `⊢ curl "https://data.nasa.gov/resource/y77d-th95.json" > meteorites.json`

But it's pretty hard to read, since it's compressed and pretty big for a text file.
<details>
<summary>Here, judge for youself:</summary>

`⊢ cat meteorites.json | head -n 3`

```json
[{"name":"Aachen","id":"1","nametype":"Valid","recclass":"L5","mass":"21","fall":"Fell","year":"1880-01-01T00:00:00.000","reclat":"50.775000","reclong":"6.083330","geolocation":{"type":"Point","coordinates":[6.08333,50.775]}}
,{"name":"Aarhus","id":"2","nametype":"Valid","recclass":"H6","mass":"720","fall":"Fell","year":"1951-01-01T00:00:00.000","reclat":"56.183330","reclong":"10.233330","geolocation":{"type":"Point","coordinates":[10.23333,56.18333]}}
,{"name":"Abee","id":"6","nametype":"Valid","recclass":"EH4","mass":"107000","fall":"Fell","year":"1952-01-01T00:00:00.000","reclat":"54.216670","reclong":"-113.000000","geolocation":{"type":"Point","coordinates":[-113,54.21667]}}
```

```bash
⊢ du -h meteorites.json
244K    meteorites.json
```

</details>

To make it easier for us to read we can pretty-print it: `jq '.' meteorites.json`.
<details>
<summary>Output:</summary>

```bash
⊢ jq '.' meteorites.json | head -n 19
[
  {
    "name": "Aachen",
    "id": "1",
    "nametype": "Valid",
    "recclass": "L5",
    "mass": "21",
    "fall": "Fell",
    "year": "1880-01-01T00:00:00.000",
    "reclat": "50.775000",
    "reclong": "6.083330",
    "geolocation": {
      "type": "Point",
      "coordinates": [
        6.08333,
        50.775
      ]
    }
  },
```

</details>

However, this does't solve the problem with the size, so let's also select just the first object in that array: `jq '.[0]' meteorites.json`
<details>
<summary>Output:</summary>

```bash
⊢ jq '.[0]' meteorites.json
{
  "name": "Aachen",
  "id": "1",
  "nametype": "Valid",
  "recclass": "L5",
  "mass": "21",
  "fall": "Fell",
  "year": "1880-01-01T00:00:00.000",
  "reclat": "50.775000",
  "reclong": "6.083330",
  "geolocation": {
    "type": "Point",
    "coordinates": [
      6.08333,
      50.775
    ]
  }
}
```

</details>

Now that we understand what the schema is roughly, we can get to the fun part.

We can use `.field` syntax to access object fields, `.[n]` to access array elements, and pipes `op1 | op2` to chain the results of the computations.

```bash
⊢ jq '.[0] | .geolocation' meteorites.json
{
  "type": "Point",
  "coordinates": [
    6.08333,
    50.775
  ]
}
```

`jq` also includes a lot of other features, like comparisons `==`,`!=` and filters `select`.
You can check the [documentation](https://stedolan.github.io/jq/manual/) and the [tutorial](https://stedolan.github.io/jq/tutorial/) for more details, for now let's play a bit more with the data.

So, given a list of meteorites with all coordinates, we can list all the meteorites that fell in the Netherlands.
Of course, checking precise bounds is going to be hard, so let's just do a bounding box from [humdata.org](https://data.humdata.org/dataset/bounding-boxes-for-countries/resource/aec5d77d-095a-4d42-8a13-5193ec18a6a9):  
Latitute: from 50.75 to 53.685  
Longtitute: from 3.113 to 7.217

Then we proceed as follows:

1. Filter out the entrances without latitute and longtitude.
2. Filter out by latitute.
3. Filter out by longtitude.
4. Select the `name` field for those which satify the conditions above

```bash
⊢ jq '.[] | select (.reclat != null and .reclong != null) | select(.reclat | tonumber | (50.75 < .) and (. < 53.68)) | select (.reclong | tonumber | (3.13 < .) and (. < 7.21)) | .name' meteorites.json
```

Drumroll:

```bash
"Aachen"
"Ellemeet"
"Glanerbrug"
"Ramsdorf"
"St. Denis Westrem"
```

Our data suggests that there are at least five.
However, the name of the first one seems suspiciously German and we used a bounding box, not exact border.
And there it is, if we double-check on [https://www.lpi.usra.edu/meteor/metbull.php](https://www.lpi.usra.edu/meteor/metbull.php) it turns out that only the second and third did fall in the Netherlands (Aachen and Ramsdorf were in Germany and St. Denis Westrem in Belgium).

If you want to play with `jq` a bit more, here's a couple of things to try:

* Find a bounding box for the EU and run the same check with new boundaries.
* [https://gist.github.com/graydon/11198540](https://gist.github.com/graydon/11198540) provides bounding boxes in JSON format, write a jq filter that extracts bounding box for NL.

### Documentation and more

As mentioned above, there's a [tutorial](https://stedolan.github.io/jq/tutorial/), which introduces some of the basic features of the tool.
Then there's official [documentation](https://stedolan.github.io/jq/manual).
And finally, you can play with `jq` in your browser on [jqplay.org](https://jqplay.org/).

## Task description

You're going to implement a clone of `jq` in Haskell.
However, it's a big and mature project, so we have to trim it down to be feasible to implement in ~30 hours.
Below you'll find a list of requirements for your implementation.
However, for the most part they are just a brief descriptions -- consult the documentation linked above and play with `jq` to pin down exact semantics.

### Project structure

To get you started, this repository provides
a basic template for the project. The `src` folder contains all the source files needed of the program. The program is split into the library (`Jq`), which contains all the code and a an executable (`exe`), which simply runs it. The template code you are given already contains the functionality to parse `null` as an input and the identity `.` as a filter.

- `JSON.hs` contains a datatype `JSON` to represent JSON data. It only has a single constructor `JNull`, so you will need to extend it with additional constructors to represent all kinds of `JSON` data.
- `Filters.hs` contains a datatype `Filter` to represent `jq` filters. It already has constructors for all basic filters, so you do not need to extend it yourself.
- `Compiler.hs` contains the function `compile` that transforms a `Filter` into a function of type `JSON -> Either String [JSON]`, that can be executed on `JSON` values to produce either an error `String` or a list of results. It is currently only implemented for the `Identity` filter, so you will need to add cases for the other filters.
- `CParser.hs` and `JParser.hs` contain functions `parseFilter` and  `parseJSON` for parsing filters and `JSON` data, respectively. They both make use of the monadic parsing library from Chapter 13 of *Programming in Haskell (second edition)* by Graham Hutton. The code from this chapter can be found in the file `Parsing/Parsing.hs`. The functionality of `CParser.hs` and `JParser.hs` is re-exported by the module `Parser.hs`.
- Finally, `Main.hs` contains the `main` function that collects the inputs, compiles the filter and runs it. You do not need to edit it yourself.
  * `Parsing` contains parsing library by Graham Hutton.
The `test` directory contains some tests for you to run. If you want to add more tests of your own, you only need to edit `data/jq.test`.

You are free to add additional modules to your project should you need them,
but we ask you to keep the existing structure intact to make grading easier.

In addition to the functions from the Haskell prelude, you are allowed to use
functions from the following packages:

- `containers` (https://hackage.haskell.org/package/containers)
- `QuickCheck` (https://hackage.haskell.org/package/QuickCheck)

If you want to use these packages, you should uncomment the corresponding
line(s) in `JqClone.cabal`. Using other packages or copy-pasting code you found
online is not allowed.

### Base project

This section describes the minimum functionality we expect your implementation to satisfy.

*Grading*. In this section you can earn a maximum of 75 points, which constitutes 75% of your final grade for the project. "0 points" means that the feature is already implemented in the template. You don't *have* to do tasks above in any particular order, but we feel like this order corresponds to escalating difficulty in implementation.

1. (0 points) Read JSON input from STDIN and filters as the first argument.  
  `echo '{"this" : "that"}' | jq '.'`
2. (15 points) Parse and pretty-print valid JSONs.  
   This means, that your implementation should be able to handle  
   * `null`
   * numbers (floating-point `1.0` and E-notation included `1.0E+20`)
   * strings (with support for escape characters `"Hello, \"world\"!"`)
   * Booleans
   * Arrays
   * JSON Objects
   
   *Hint*. Add constructors to the `JSON` type in `src/Jq/Json.hs` and define a parser for each constructor in `src/Jq/JParser.hs`
   
   For full formal definition of JSON take a look at:
   * The [wiki](https://en.wikipedia.org/wiki/JSON)
   * The [rfc](https://tools.ietf.org/rfc/rfc8259.txt)

3. (37 points total) Implement all [basic filters](https://stedolan.github.io/jq/manual/#Basicfilters).  
   In particular:  
   1. (0 points) Identity filter `.`, which returns an object given to it.
   2. (1 point) Parenthesis '()', used for grouping operations.
   3. (4 points) Object indexing, both identifier `.field` and generic `.["field"]`.  
      If the field doesn't exist, running the filter should return `null`.
   4. (3 points) Optional object indexing `.field?` (and `.["field"]?`), which doesn't rise an exception if the value indexed into isn't an object.
   5. (4 points) Array index and slice `.[0]`, `.[0:10]`.  
     Slices behave very similarly to Python or Go.
   6. (6 points) Array/Object Value Iterator `.[]`, `.[1,2,3]`.  
     When applied to an array, the `.[]` filter iterates over its elements, and when applied on an object it iterates over its values (*not* over the keys).
     `.[1,2,3]` returns an iterator which goes over the first, second and third elements.
   7. (4 points) Optional counterparts for indexing, slicing and iterators.
   8. (7 points) Comma operator `op1 , op2`.  
     Returns results of both `op1` and `op2` akin to iterator elements.
   9. (8 points) Pipe operator `op1 | op2`.  
     Passes results of `op1` into `op2`.
   
   *Hint*. for each basic filter, add a constructor to the `Filter` type in `src/Jq/Filters.hs`, then define parser for it in `src/Jq/CParser.hs`, and interpret the filter into Haskell code by adding a case to the `compile` function in `src/Jq/Compiler.hs`

4. (23 points total) Value constructors  
   1. (9 points) Simple value constructors.  
     `jq` allows you to construct values from the input elements:
     `echo '1' | jq '{"this" : [.]}'` (this produces `{"this": [1]})`), or ignoring them:
      `echo 'null' | jq '{"this" : [42]}'` (this produces `{"this": [42]})`).
      For this task you're asked to implement only the "simple" ones: numbers, booleans, strings, arrays without iteration (`[1,2,.field]`, not `[.[]]`), objects
   2. (14 points) More complex value constructors  
      This is complementary to the previous subtask -- implement the constructors for arrays (for example `[.items[].name]`, objects (for example `{user}`).
      Be warned that this part is harder than it seems and some features interact in a non-obvious way, and not every aspect of behaviour is described precisely in the documentation.
      In case of doubt, you can experiment with the reference implementation and follow what it does.



### Advanced tasks

To get your grade to 100% your implementation can also include the following
features. The order and the number of points corresponds to the expected
difficulty in implementation. Each point is worth 1 percent of the final grade,
but the total grade is capped at 100%. Please note that the tasks in this
section require the basic functionality from the previous section to be already
implemented, so it does not make sense to start on these advanced features
before you are confident in your implementation of the basic part.

* (5 points) [Recursive descent operator](https://stedolan.github.io/jq/manual/#RecursiveDescent:..) `..` iterates over all sub-values of the current value, including itself.
  For example, `echo [{"a" : 1}] | jq '..'` results in

  ```json
  [
    [
      {
        "a": 1
      }
    ],
    {
      "a": 1
    },
    1
  ]
  ```
  In order for this subtask to count your implementation should handle all JSON and have all basic filters.

* (10 points) [Conditionals and comparisons](https://stedolan.github.io/jq/manual/#ConditionalsandComparisons):
  * "Equal" and "not equal" operators `==`, `!=`, which take two JSON values and output a Boolean.
  * If-then-else expression `if A then B else C end`.
  * Comparison operators for numbers `<`, `<=`, `>`, `>=`
  
   In order for this subtask to count your implementation should handle all JSON values and have all basic filters.

* (10 points) Arithmetic expressions: `+,-,*,/`, described [here](https://stedolan.github.io/jq/manual/#Builtinoperatorsandfunctions).  
  Mind that these operations operate not only on numbers, but also on other JSON values such as arrays, strings, and objects.  
  In order for this subtask to count your implementation should handle all JSON values, have all basic filters, and simple object constructors.

* (10 points) [Try-catch expressions](https://stedolan.github.io/jq/manual/#try-catch) `try op1 catch expr` which tries to execute `op1` and if exception appears, returns `expr`.  
  In order for this subtask to count your implementation should handle all JSON values and have all basic filters.

* (10 points) [Syntactic variables](https://stedolan.github.io/jq/manual/#Variable/SymbolicBindingOperator:...as$identifier|...) `expr as $id | op`, which allow you to bind the value expr to identifier `id` before passing it further to `op`.  
  In order for this subtask to count your implementation should handle all JSON values and have all basic filters.

* (15 points) [Reduction operator](https://stedolan.github.io/jq/manual/#Reduce) `reduce`, which corresponds to a fold over results returned by the previous operation.  
  In order for this subtask to count your implementation should handle all JSON values and have all basic filters.

* (15 points) [Functions](https://stedolan.github.io/jq/manual/#DefiningFunctions), which allows you to define syntactical functions in jq filters.  
  In order for this subtask to count your implementation should handle all JSON values, have all basic filters, and simple object constructors.

## Getting started

In general you should limit the number of external libraries used to possible minimum.
You are encouraged to use [containers](https://hackage.haskell.org/package/containers) package for maps (dictionaries), [Parsing.hs](www.cs.nott.ac.uk/~pszgmh/Code.zip) from chapter 13 of [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html) for parsing.
The latter is already available as `Parsing.Parsing` module.

1. Clone repository [https://gitlab.tudelft.nl/bliesnikov/jq-clone/](https://gitlab.tudelft.nl/bliesnikov/jq-clone/). 
2. Put your name and email in `JqClone.cabal`.
3. Run `stack build` to build your project and `stack install` to install the `jq-clone` executable.
4. To run your implementation use `echo <input.json> | jq-clone -- <filter>`

### Testing

We also provide a small test suite for you to verify the correctness of your implementation.
Test cases are in `test/data/jq.test`, which is extensible.
You can test your current implementation with `stack test` (if you're using `cabal` and `cabal test` yields nothing run `cabal configure --enable-tests` first).
You have to have `jq` installed on your machine and available on your $PATH for this -- tests use it for pretty-printing.

### Getting help

If you are unsure where to start or feeling a bit lost, you can approach the project in the following manner:

1. Write a parser for numbers and strings.  
   Maybe skip floating point and escape characters for now -- you can always add them later.
2. Extend the parsing to handle objects.
3. Implement object indexing for filters.
4. Implement pipe and comma operators.
5. Implement arrays and array operations.
6. Add value construction and make sure that composed operations have the right semantics.
7. Proceed with the rest of the assignments.

If you have questions about a part of the project, you can create a question on
Stack Overflow at
[stackoverflow.com/c/tud-cs/](https://stackoverflow.com/c/tud-cs/). Remember to
phrase your question in general terms and avoid including part of your code.
Alternatively, you can ask questions to one of the teaching assistants during
the weekly lab sessions, or send an email to
[fp-cs-ewi@tudelft.nl](mailto:fp-cs-ewi@tudelft.nl).
