Plate Scraping
==============

This is a screenscraper of the danish sites trafikstyrelsen.dk and tinglysning.dk for licence plate information.

I'm using Haskell and TagSoup for the job. Works fine for now.


Acknowledgements
----------------

None yet. But I'll probably think of some later on. :)


Installation
------------

This runs as a REST service on Scotty. Just do a:

```bash
cabal build
```

- and then run the resulting program.

NOTE: A separate project runs the program in a docker container. See platedocker for build instructions.



