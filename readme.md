Plate Scraping
==============

This is a screenscraper of the danish sites trafikstyrelsen.dk and tinglysning.dk for licence plate information.

I'm using Haskell and TagSoup for the job. Works fine for now.

*Not* using Network.HTTP.Client.TLS - that lib was full of bugs - but Network.HTTP.Client.OpenSSL.

To build with OpenSSL headers on Mac OSX Capitan you need these steps:

brew update
brew install openssl
cd /usr/local/include
ln -s ../opt/openssl/include/openssl .

- and finally you need these includes and libs added to stack.yaml:

extra-include-dirs:
- /usr/local/opt/openssl/include
extra-lib-dirs:
- /usr/local/opt/openssl/lib

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



