minipaste, a minimal pastebin service
=====================================

Minipaste is a minimal pastebin service built with haskell.
It is built on top of scotty, hasql, highlighting-kate and blaze.


Features
--------

- List pastes (all/by lang)
- POST a paste (baseurl/:lang)
- GET a paste (baseurl/:uuid)
- PATCH a paste' lang (baseurl/:uuid/:lang)

TODO
----

- Better encoding handling


TODON'T
-------

- edit a paste, as the url of the paste is the UUID v5 hash of its contents.

