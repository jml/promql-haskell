# promql-haskell

Experimental, work-in-progress implementation of the PromQL language, as used
and defined by [Prometheus](https://github.com/prometheus/prometheus).

Absolutely not ready to be used yet.

## To be done

- [ ] Write a more-or-less complete parser
- [ ] Separate lexing from parsing (if this helps with performance, error messages or code clarity)
- [ ] Experiment with using Parsec's language definition features rather than
  manual parsing
- [ ] Use this library to write a linter for PromQL
- [ ] Use Prometheus metric naming conventions to type check PromQL expressions and statements
- [ ] Add a pretty-printer: https://hackage.haskell.org/package/wl-pprint-text

### Boring stuff

- [ ] LICENSE appropriately
- [ ] CI
