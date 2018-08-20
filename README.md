# ob-erlang
Org-Babel support for evaluating erlang source code.

## Usage
Clone repository and put ob-erlang.el in your Emacs load path.

### Clone

```bash
git clone https://github.com/xfwduke/ob-erlang
```

### Put in path
Add ob-erlang.el to your load path. Example:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-packages")
```
Then copy file ob-erlang.el to "~/.emacs.d/site-packages"

### Configure Babel

```elisp
(require 'ob-erlang)
(org-babel-do-load-languages
    'org-babel-load-languages
    '((erlang . t)))
```

### Additional Code Block Parameters

#### :module

Specify module name for erlang code.
It will be omitted if the code contained "-module(ModuleName)."

#### :start

Specify entry function to eval erlang code.
Erlang's default entry function is start/0.
