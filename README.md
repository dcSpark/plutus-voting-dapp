<h1 align="center">
  Plutus Voting Smart Contract
</h1>
<p align="center">A smart contract which enables users to vote for spending funds from a treasury UTXO.</p>

<p align="center"><img src="https://img.shields.io/badge/license-mit-blue?style=for-the-badge&logo=none" alt="license" /></p>

## Disclaimer

The code on this repository has **not** been audited. We don't recommend it using in production without a full security audit. Use it at your own risk!.

## Protocol

This contract allows to pay the content of a treasury to the an address that gets enough votes.
A special designated token represents the vote. Depening on the intial voting token distribution, allows different voting power per wallet.

Protocol steps:

0. Preparation steps:
    - Mint voting tokens and distribute among participating wallets.

1. Set up the treasury:
    - Lock tokens in the treasury with the vote configuration (voting token and required quorum)

2. Vote for address(es):
    - Locks the a number of vote tokens together with a datum containing the voted choice.

3. Tally votes:
    - Tries to collect more utxos with more than the required quorum on a particular choice.
    - If successful, pays the content of the treasury to the winniing address.

4. Return (or recall) vote
    - At any time, a voter can get back all their votes

## Building

To build the project execute `cabal build` at the project root.

To build:

``` bash
$ nix-shell
...
$ cabal build
...
```

## Testing

To run use-case test execute the following commands at the project root.

``` bash
$ cabal test
Build profile: -w ghc-8.10.4.20210212 -O1
In order, the following will be built (use -v for more details):
 - voting-dapp-0.1.0.0 (test:voting-dapp-test) (first run)
Preprocessing test suite 'voting-dapp-test' for voting-dapp-0.1.0.0..
Building test suite 'voting-dapp-test' for voting-dapp-0.1.0.0..
Running 1 test suites...
Test suite voting-dapp-test: RUNNING...
use cases
  Simple endpoint tests
    Expose endpoints:                                                OK
    Build treasury:                                                  OK (0.02s)
    Single vote:                                                     OK (0.04s)
    Return vote:                                                     OK (0.08s)
    Tally votes:                                                     OK (0.12s)
  Voting scenarios
    Voted wallet with quorum should get content of treasury:         OK (0.30s)
    Voted wallets with no quorum should not get content of treasury: OK (0.17s)

All 7 tests passed (0.74s)
Test suite voting-dapp-test: PASS
```
