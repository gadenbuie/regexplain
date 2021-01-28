## regexplain 0.2

### 0.2.2.9000 (will be 0.3.0)

* Rewrote regexp-matching internals, the RStudio addin can now display global
  regexp matches.
  
* All app-related functions are now prefixed with `regexplain_`. In particular,
  `regex_gadget()` was renamed in favor of `regexplain_gadget()`.
  
* `regexplain_gadget()` accepts both `text` and `pattern` arguments, so you can
  easily load text or regular expressions from your R session into the gadget.
  If you're working with designing a regular expression, this lets you move
  back and forth between the gadget and your code much more easily.

* Fixed design of the RegEx tab so that inputs, options and output will no
  longer overlap when the gadget is open in a small viewer pane.
  
* Clarified meaning of `\W`, `\D`, and `\S`, thanks @GegznaV.

* Moved `stringr` and `rematch2` to Suggests.

* `view_regex()` now returns the HTML and dependencies directly, making it 
  easier to include the regex visualization in R Markdown documents. The CSS
  selectors used in the `view_regex()` HTML are now more specific to avoid
  potential selector collisions.
  
* `regexplain_gadget()` now also returns the built regexp directly, enabling
  usage like `pattern <- regexplain_gadget()`.
  
* Fixed an issue with `shiny` version 1.6 (thanks @jthomasmock, #22)

### 0.2.2

* Fix issues with coloring of groups in regexplain gadget when matched groups
  start and end at the same index, especially when there are zero-length
  groups (thanks, @HanOostdijk).

* Reduce package size by optimizing images and offloading the screencast gifs to
  the `assets` branch.
  
* Use of `knitr` is automatically detected when calling `view_regex()` inside
  an R Markdown document. The `knitr` parameter of `view_regex()` has been
  removed. Added `result_only` parameter for interactive viewing. As a side
  effect, the group coloring CSS is automatically embedded in the HTML output.

### 0.2.1

* Fix addin crash when replacement function is visited but replacement field
  is not initiated (#8).

### 0.2.0

* Added "Library" button that opens a dialog to import regex patterns sourced 
  from <https://projects.lukehaas.me/regexhub> and 
  <https://github.com/trinker/qdapRegex>. The library is stored in
  `inst/extdata/patterns.json`.
* Added "Introduction", "Try These Examples" and "Resources" tabs to
  the standard gadget Help tab.
* Several bugfixes and tests were added around unicode character escaping within
  the gadget. 
* Enable `perl` mode by default in gadget (consistent with `stringr` and most
  regexes in the wild, but not base R). Write out selected options as comments
  when not consistent with base R when returning pattern from gadget.
* Rewrote Readme with new screenshots and screencasts.
* Added options `regexplain.addin.max_lines` and options for debugging gadget inputs.


## regexplain 0.1.x

### 0.1.5

* Options from **Regex** tab are extended to **Output** tab as well. Colored
  options according to group where they are applied.
* Added a mechanism to announce package updates while this package is on GitHub.
  Only checked once per R session on the first launch of the gadget.

### 0.1.4

* Added string substitution function to **Output** tab (`g?sub`, 
  `str_replace(_all)?`) with replacement field
* `view_regex()` output now differentiates between capture and non-capture groups
* Fix typos (thanks @katrinleinweber) and tweak styles
* Added a `NEWS.md` file to track changes to the package.
