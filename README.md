minipaste, a minimal pastebin service
=====================================

Minipaste is a minimal pastebin service built with haskell.
It is built on top of scotty, postgresql-simple, highlighting-kate and blaze.


Features
--------

- POST a paste (baseurl/:lang)
- GET a paste (baseurl/:uuid)
- PATCH a paste' lang (baseurl/:uuid/:lang)

TODO
----

- List pastes (all/by lang)

TODON'T
-------

- edit a paste, as the url of the paste is the UUID v5 hash of its contents.

