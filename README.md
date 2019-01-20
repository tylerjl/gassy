# [gassy][]

Gassy is a simple way to log gas receipts for later analysis in Elasticsearch.
It runs on [magicbane][], which is a Haskell framework akin to sinatra or flask.

Sorry about the name.

## Quickstart

You need to have [stack][] installed.

```sh
$ stack build --exec gassy
```

Gassy will talk to `https://localhost:9200` for Elasticsearch.
Set `ELASTICSEARCH_HOST` to use a different Elasticsearch endpoint.

## Development

I haven't written tests or benchmarks because the compiler has been good enough so far.

To see an example of what this looks like with Spock, see commit 43ce96d6dd37052fd85012923a161a869ac0a564.
I switched to magicbane as an experiment, and Servant has felt a little better.

[gassy]: https://github.com/tylerjl/gassy
[magicbane]: http://hackage.haskell.org/package/magicbane
[stack]: https://docs.haskellstack.org/en/stable/README/
