# Web Scraping Engine

## Usage

To run:

```
stack exec example --cache-dir cache -a user-agents.txt -o output.csv
```

During testing/development, you can run the scraper from within GHCI:

  * `cd example`
  * `stack ghci`
  * `mainTest "--cache-dir cache --cache-only -a user-agents.txt -o output.csv"`

To run the scraper with anonymization:

  1. `cd example`
  2. `bash build-proxies.sh > torrc-file`
  3. `tor -f torrc-file &` (wait until logs report success)
  4. `stack exec example -- --cache-dir cache -a user-agents.txt --torrc torrc-file o outdata.csv -m 8111 +RTS -N15` where
    * `8111` is the port to an EKG monitor on `localhost`
    * `-N15` is how many cores to use
  5. After a long time you will need to kill the process manually.

## Development

Develop with one of:

  * `stack ghci`
  * `nix-shell --run 'cabal repl'`

Build with one of:

  * `stack build`
  * `nix-shell --run 'cabal build'`
  * `nix-build`

