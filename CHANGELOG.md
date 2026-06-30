# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- support for OTP 25, 26, 27, 28 and 29
- API documentation through ex_doc (EEP-48 doc chunks)

### Changed

- [dep.] `iso8601` requirement loosened to `~> 1.3` (1.3.3 no longer compiles
  on OTP 27+, which reserves the `maybe` keyword)
- CI moved to a modern GitHub Actions matrix (OTP 24 through 29)
- development tooling: `erlfmt`, `rebar3_hank`, `rebar3_lint` and `elvis`

### Removed

- **CI coverage for OTP 22 and 23** (declared minimum is now effectively OTP 24)

## [1.0.0] - 2021-08-25

### Added

- support for OTP 24
- CHANGELOG.md
- CI through GitHub Actions

### Changed

- [dep.] `iso8601` from 1.2.3 to 1.3.3

### Removed

- **support for OTP 21**
- **support for OTP 20**
- **support for OTP 19**
- **support for OTP 18.3**
- [dep.] `stacktrace_transform`
- Travis-CI
- Circle-CI

## [0.2.5] - 2019-01-19

### Added

- Travis-CI targets for OTP 20.2, 20.3, 21.0, 21.1 and 21.2
- GitLab mirror link in the Hex package metadata

### Fixed

- missing build rule dependencies
- concurrency errors when building with recent versions of GNU make

## [0.2.4] - 2018-08-17

### Fixed

- build errors on OTP 21

## [0.2.3] - 2017-11-27

### Fixed

- decoding of high 1-, 2- and 4-byte integers

## [0.2.2] - 2017-10-11

### Fixed

- memory leaks of decoded binary data

## [0.2.1] - 2017-10-11

### Added

- support for building under Erlang/OTP 17.x and 18.x

## [0.2.0] - 2017-10-11

### Added

- XML plist decoding
- UID decoding (both binary and XML formats)
- set decoding, with element uniqueness enforced
- Unicode / non-latin1 text support in the XML decoder
- detection of cycling references when decoding binary plists
- a convenient way of launching a local shell

### Changed

- stricter handling of text when decoding binary plists

### Fixed

- wrong wrapping of the root element when decoding XML

## [0.1.0] - 2017-10-10

### Added

- initial release: binary plist decoding, test data, LICENSE and README
