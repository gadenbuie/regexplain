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