regexplain
================

[![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/master)
![](https://img.shields.io/badge/lifecycle-needs_testers-yellow.svg)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/regexplain)](https://cran.r-project.org/package=regexplain)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--03--12-yellowgreen.svg)](/commits/master)

<!-- Links -->

regexplain is an RStudio addin that helps you interactively build up
your regex expressions. Inspired by [RegExr](https://regexr.com/) and
`stringr::str_view`.

## Installation

Installation is easy with `devtools`:

``` r
devtools::install_github("gadenbuie/regexplain")
```

## Status

Mostly working, but there [may be issues or future changes](#issues).

I would love your help testing this, feel free to send me your feedback
on Twitter at [@grrrck](https://twitter.com/grrrck) or through the
[issue tracker](https://github.com/gadenbuie/regexplain).

## RStudio Addin

![regexplain screencast](docs/gadget-screencast.gif)

The main feature of this package is the RStudio Addin **Regexplain
Selection**. Just select the text or object containing text (such as the
variable name of a vector or a data.frame column) and run **Regexplain
Selection** from the RStudio Addins
dropdown.

<img src="docs/rstudio-addin-list.png" width = "250px;" alt="regexplain in the Rstudio Addins dropdown">

The addin will open an interface with 4 panes where you can

  - edit the **text** you’ve imported
  - build up a **regex** expression and interactively see it applied to
    your text
  - test the **output** of common string matching functions from `base`
    and `stringr`
  - and refer to a **help**ful cheatsheet

![The panes of regexplain](docs/regexplain-gadget-tabs.png)

When you’re done, click on the **Send Regex to Console** to send your
regex expression to… the console\!

``` r
> regex <- "(is|were|was) ([[:alpha:]]+) ([[:alpha:]]+)"
```

### Additional Addins

There are two more addins. **Regexplain File** lets you import the text
lines from a file containing the text you want to process with regular
expressions. **Regexplain Cheatsheet** opens the help page in the Viewer
pane without blocking your current R session.

## View regex results without the interactivity

regexplain also provides the function `view_regex()` that you can use as
a `stringr::str_view()` replacement. In addition to highlighting matched
portions of the text, `view_regex()` also colorizes groups and attemps
to colorize the regex expression itself as well.

``` r
text <- c("breakfast=eggs;lunch=pizza",
          "breakfast=bacon;lunch=spaghetti", 
          "no food here")
pattern <- "((\\w+)=)(\\w+).+(ch=s?p)"

view_regex(text, pattern)
```

![Example `view_regex(text, pattern)`.](docs/view-regex.png)

``` r
t_nested <- "anestedgroupwithingroupexample"
r_nested <- "(a(nested)(group(within(group))(example)))"
view_regex(t_nested, r_nested)
```

![Example of nested groups](docs/view-nested.png)

## Known Issues and Future Work

Regular expressions are nothing if not a collection of corner cases.
Trying to pass regular expressions through Shiny and HTML inputs is a
bit of a labrynth. For now, assume any issues or oddities you experience
with this addin are entirely my fault and have nothing to do with the
fine packages this addin is built on. If you do find an issue, [please
file an issue](https://github.com/gadenbuie/regexplain).

#### Things I know are wonky

  - Things get weird when regular text is surrounded by capture groups,
    i.e. `"(\w+) not grouped (\w+)"`.

  - `view_regex()` colorizes non-capture groups. I forgot about them
    when building that feature, I’ll get back to it.

#### Notes

  - I’ve set up this app so that most escape sequences don’t need to be
    escaped. For example, you can enter `\w`, whereas in R this would
    need to be stored as `"\\w"`. The regex returned by the gadget will
    include the double backslash. In these cases the text input is not
    escaped by Shiny.
    
    Unicode and hex escape characters also do not need to be escaped,
    thanks to `stringi::stri_unescape_unicode()`. Here, `"\u"` *is*
    escaped by Shiny so I had to make sure they are unescaped. The list
    of escaped characters that get unescaped is `"\\u|\\x|\\N|\\a|\\o"`,
    please let me know if you find any others that should be on this
    list.

#### Planned improvements

  - I may add `stringi` functions to the list of available functions in
    the **Output** tab.

  - I would like to add the regex/function options for the functions in
    the **Output** tab, similar to the options present in the **Regex**
    tab.
