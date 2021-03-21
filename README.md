# ob-erlang
Org-Babel support for evaluating erlang source code.

## Usage
Clone repository and put `ob-erlang.el` in your Emacs load path.

### Clone

```bash
git clone https://github.com/xfwduke/ob-erlang
```

### Put in path
Add `ob-erlang.el` to your load path. Example:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-packages")
```
Then copy file `ob-erlang.el` to `~/.emacs.d/site-packages`

### Configure Babel

```elisp
(require 'ob-erlang)
(org-babel-do-load-languages
    'org-babel-load-languages
    '((erlang . t)))
```

### Additional Code Block Parameters

`-compile(export_all).` will be auto inserted into code during compile phase if no `-export([]).` line in the code.

#### :module

Specify module name for erlang code.
It will be omitted if the code contained `-module(ModuleName).`

```org
#+BEGIN_SRC erlang :module tryerlang
start() ->
	io:format("hello world").
#+END_SRC

#+RESULTS:
: hello world
```

If no module name is specified by any of the above means, then a
default module name of `m` is used.

#### :start

Specify entry function to eval erlang code.
Erlang's default entry function is start/0.

```org
#+BEGIN_SRC erlang :module notstart :start main
main() ->
	io:format("entry is main/0").
#+END_SRC

#+RESULTS:
: entry is main/0
```

#### with out :module and :start

```org
#+BEGIN_SRC erlang
-module(sayhello).
start() ->
	io:format("say hello").
#+END_SRC

#+RESULTS:
: say hello
```

#### :name

Specify erlang node long-name

#### :cookie

Specify cookie for -setcookie
