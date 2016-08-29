Plate Scraping
==============

This is a screenscraper of the danish sites trafikstyrelsen.dk and tinglysning.dk for licence plate information.

I'm using Haskell and TagSoup for the job. Works fine for now.

I'm *not* using Network.HTTP.Client.TLS - that lib was full of bugs. I'm using Network.HTTP.Client.OpenSSL instead.

To build with OpenSSL headers on Mac OSX Capitan you need these steps:

```bash
brew update
```
```bash
brew install openssl
```
```bash
cd /usr/local/include
```
```bash
ln -s ../opt/openssl/include/openssl .
```
Finally you need these includes and libs added to stack.yaml:

```bash
extra-include-dirs:
- /usr/local/opt/openssl/include
extra-lib-dirs:
- /usr/local/opt/openssl/lib
```

Acknowledgements
----------------

None yet. But I'll probably think of some later on. :)


Installation
------------

This runs as a REST service on Scotty. Just do a:

```bash
stack build
```

... and then run the resulting platescraping-exe program.

NOTE: A separate project runs the program in a docker container. See platedocker for build instructions.



