# HiTables
A source-to-source compiler for ipTables rulesets. It applies the concept of HiCuts[1] to ipTables rulesets to optimize packet classification time. This code was an early experiment which lead to my bachelor thesis "Firewall Ruleset Transformations for netfilter/iptables" and set the foundation for the software used in the paper [Trees in the List: Accelerating List-based Packet Classification Through Controlled Rule Set Expansion](http://ti-publications.informatik.hu-berlin.de/publications/167/)

[1]
HiCuts: http://klamath.stanford.edu/~pankaj/paps/hoti99.pdf

## Get it to run

> cabal update && cabal install

## Tests

> cabal configure && cabal test
