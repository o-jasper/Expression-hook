# Expression hook

Expression hook is about getting more grip on your code. Features:

**Expression-hook** is a hook on *every* expression, knowing in what 
function.

**Expression scan** gathers statistics, information for autodocumentation.

**Package-project** determines dependencies, makes .asd files for you, 
standardizes some stuff. Provides the ability to do other things while 
packaging. (Which can be later filled with autodocumentation and such.)

## Project contains

* Expression hook, a function called on every expression.
  * Needs the scanned code to be availabe for running since it needs the 
    macros.
  * It might trip on stuff meant to be read with `readtable`, probably not 
    bug free.
  * TODO is to try/test the 'adapters' to be able to use `*macroexpand-hook*`
    also.

* Expression scan, which uses to hook to collect a bunch of data.
  * Symbol-dependencies between packages, unfortunately does not detect all 
    dependencies,
    as generics can be extended by `defmethod` without connecting any symbol.
  * Dependencies between functions, dependence of functions on variables.

* package-project which generates .asd files, and standardizes folder
   organization.
  * NOTE: for one, `defmethod` can make packages depend on each other that is
    *not* detected! This is because it is possible to not refer to each
     others symbols.
  * note: system names not corresponding to package names need to be 
    reported. For instance :alexandria.0.dev is defaultly mapped to 
    alexandria. See `package-project:package-info`.

* Two packages that provide functions for to package-project allowing
  autodocumentation to be applied automatically, via an `:also` keyword.
  (See more later one of documentation-template, one gil-autodoc)

* gil-autodoc, autodocumentation in.. gil.. I haven't made put any gil out 
  there. Note that a lot of the documentation strings have markup like stuff 
  in them.(Hoping to later add markup to autodocumentation)

## Dependencies

The asd files describe this, of course.

* The other github project [j-basic](https://github.com/o-jasper/j-basic). 
  (`git@github.com:o-jasper/j-basic.git`)

* [alexandria](http://common-lisp.net/project/alexandria/) used throughout. 
* [cl-fad](http://weitz.de/cl-fad/). Probably should get rid of this 
  dependency, i think some functionality overlaps with alexandria.
* As said, gil-autodoc depends on unreleased stuff(gil-def,gil-html) at the 
  moment. So everything based on those is a bit moot. Hopefully i will fix 
  that soon.

## Usage of package-project

Make sure you **backup stuff** to be sure! Then we of course need the 
`:package-project` package available. (For instance by `require`-ing it)

Make the directories `your-projects/the-project/src/`, the main .lisp files in
`src/` otherwise things are categorized.(Last collumn is what the package name
should precede with)

    tests                                               src/test/           test-
    examples                                            src/example/        example-
    'messing about'(since a mess, might not have .asd)  src/try/, src/mess  try-, mess-
    machine readable documentation/autodoc assistance   src/doc/            doc-
    guis for stuff(unless gui is the central purpose)   src/gui/            gui-
    data 'for code'                                     src/data/           data-

You can extend the idea, but be careful so that it makes sense, preferably 
ask me to add it to this list so we dont get different-but-similar terms.
 Note that i am pretty much on purpose laying down exactly how files are 
organized.

Now you can make your lisp implementation run
 `(package-project-auto-update "yourfile.lisp")` and if 
everything goes right it should make a .asd file at toplevel of your project.

With the `:also` keyword you can provide things to 'also' do, for instance
 autodocumentation:(Check out caveits of this particular one below.)

    (package-project-auto-update "yourfile.lisp"
     :also (package-project-documentation-template:also))`

And it should be easy to create your own 'alsos' it expects a function taking
three arguments: `package` the keyword-ized package name, which can for 
instance be used to access expression-scan results.
`project-dir` tells you where it is and
`extra` the extra stuff contained in `/doc/info/info`, if that file exists.
Something like `(lambda (package project-dir extra) ...)` then. Of course,
provided alsos can be looked at as examples.

### Optional stuff
Optionally information can be put in the `doc/info/` directory

#### `info`
 there will contain a keyword-value-list that is passed to `extra` as 
told about above. 

* `:asd-license`/`:license` indicates what to fill the `:license` entry of
   the asd-file with. (Probably a good idea to provide it.)
   if a list, the first element indicates the name, the second the header as
   relative to `doc/info/` (see next section) _All_ license info goes in here.
* `:author`/`:authors` can indicate the author(s).
* `:maintainer`/`:maintainers` can indicate the maintainer(s). 
* `:package-info` is followed by a alist with different packages, specifying.
  + Overriding any of the ones above for the specific package.
  + `:system-name` for overriding the name (for instance `:alexandria.0.dev` 
	to just `:alexandria`)
  + `:also-depends` for adding more dependencies the scanner doesnt catch.
    (It will catch it if packages refer to each other namespace)

Note that the documentation string is taken from the package. It will whine
about it on the .asd file if it is missing. Don't make it too long either, 
just use separate files for extensive documentation.

#### `header`/`header.txt` 
contains a header that, if exists is **replaces** the
headers of the source files. (For instance containing copyright info.) 
Specifically, it looks for continuous 'double comments' **`;;`** at the top
 of the file. As noted above, providing `:license` with a list can override 
 it.

### Doing it quickly with emacs

For instance use this:

    (defun cl-scan-expr ()
      (interactive)
      (slime-interactive-eval
       (concatenate 'string "(package-project:auto-update \"" 
                    (buffer-file-name) "\")")))

And possibly bind it to a key combination, of course `(global-set-key "\M-s" 
'cl-scan-expr)` (Actually would be better if that was lisp-only mode) Now you
only have to press two buttons to apply it.

You might also want to add `(require :package-project)` in `~/.sbclrc`, 
otherwise it is simply not available.

### Some `:also`s provided.

Basically the only one useable is the `package-project-documentation-template`
package. It produces a ... template. You have to fill stuff in. Convenient, 
but not truly autodocumentation. **Warning:** if you run with that `:also`
it will overwrite your edits!

To also get autodocumentation use the `:also` keyword and write a function to 
insert one. For instance:

    (defun cl-scan-expr ()
      (interactive)
      (slime-interactive-eval
      (concatenate 'string "(let ((for-gil-xml-like:*tab-element* \"  \"))
      (package-project:auto-update \""
                      (buffer-file-name) "\" 
    :also (package-project-documentation-template:also)))\"")))

There is also a `:expr-hook` keyword which allows you scan at the same time as
the expression-scan scanner.

###More documentation
Autodocumentation is under `/doc/gil-autodoc/`, filled in most docstrings. The
links currently don't work, and it could be a lot better.

## TODO

* Make any linux/SBCL specific stuff less specific for it.

* Try improve success probability on package-name vs system-name mismatches..
  For instance by finding and listing more mismatches.
  ..At some point i scanned many of the stuff on my computer.. Need to do that
  again..

* expression-scan is now handling going through asd files, expression-hook or
  maybe a third package should do it so people creating other data-gatherers
  don't have to reimplement it.

* More autodocumentation `:also`s. Release something of the 'gil-project' to 
  enable people to use the autodocumentation. Perhaps make a markdown or some
  other that github can show immediately.

* Some guard against changing the header? Maybe just recommend `.gitignore` 
  and such.

* Add an extension that looks for git/svn etc. systems, and finds the 
  identifier of the current version, and stuffs it(optionally) in the
  documentation, .asd files(as comments/the `:version` keyword)

### Unrelated

The asd-versions of these can be used in for instance ~/.sbclrc to make asdf
 aware of the .asd files:

    (defun add (path)
      "Make asdf aware of single path."
      (when (string= (file-namestring path) "") 
        (unless (find (pathname path) asdf:*central-registry* :test 'equalp)
          (push (pathname path) asdf:*central-registry*)
          (pathname path))))

    (defun add-asd (path &key (asd (concatenate 'string path "asd/")))
      "If there is a `asd/` subdirectory, make asdf aware."
      (when (probe-file asd) (add asd)))

    (defun add-subdirs (path &key (prep (namestring (user-homedir-pathname))))
       "`add` all directories at this directory."
       (remove-if #'null
                  (mapcar #'add 
                         (fad:list-directory (concatenate 'string prep path)))))

    (defun add-subdir-asd (path &key (prep (namestring (user-homedir-pathname))))
      "`add-asd` all directories at this directory."
      (remove-if #'null
                 (mapcar #'add-asd
                         (fad:list-directory (concatenate 'string prep path)))))

## Copyright
Everything is under GPLv3, license included under `doc/`

## Author

Jasper den Ouden
