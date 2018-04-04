## regexplain 0.2.x

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
