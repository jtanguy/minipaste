minipaste, a minimal pastebin service
=====================================

Minipaste is a minimal pastebin service built with haskell.
It is built on top of [scotty](https://github.com/scotty-web/scotty),
[hasql](https://github.com/nikita-volkov/hasql),
[highlighting-kate](https://github.com/jgm/highlighting-kate) and
[blaze-html](https://github.com/jaspervdj/blaze-html).


Features
--------

- List pastes (all/by lang)
- POST a paste (baseurl/:lang)
- GET a paste (baseurl/:uuid)
- PATCH a paste' lang (baseurl/:uuid/:lang)

TODO
----

- Better encoding handling
- Experiment with [lucid](https://github.com/chrisdone/lucid)


TODON'T
-------

- edit a paste, as the url of the paste is the UUID v5 hash of its contents.

