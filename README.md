Show Epkg information in completion annotations
-----------------------------------------------

Out of the box [Marginalia] enriches package completion with
information from the built-in package manager.  This packages
teaches it to additionally use the [Epkgs] database, and if the
[Borg] package manager is available, then it uses information
provided by that as well.

```elisp
(with-eval-after-load 'marginalia
  (cl-pushnew 'epkg-marginalia-annotate-package
              (alist-get 'package marginalia-annotator-registry)))
```

[Marginalia]: https://github.com/minad/marginalia
[Epkgs]:      https://github.com/emacsmirror/epkgs
[Borg]:       https://github.com/emacscollective/borg
