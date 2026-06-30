# yabko

Erlang/OTP library that parses Apple Property Lists (`.plist`). Both the XML
and the binary (`bplist`) encodings are decoded through a single entry point,
`yabko:decode/1`, which auto-detects the format and returns a plain Erlang
term (maps, lists, binaries, numbers, `calendar:datetime/0`, `{uid, _}`).

## Build, test, check

```bash
make compile         # compile
make test            # eunit (+ CT, currently empty) with coverage
make check           # check-fast + check-slow
make check-fast      # format check (erlfmt) + xref + dead-code (hank) + lint (elvis)
make check-slow      # dialyzer
make format          # auto-format source with erlfmt
make eunit           # unit tests only
make doc             # EEP-48 + ex_doc HTML under doc/
make shell           # interactive REPL with the app started
```

All checks run sequentially (`.NOTPARALLEL`). CI runs `make check-fast`,
`make test`, and `make check-slow` on OTP 24–29 (Linux).

## Compiler flags

The four warnings `warn_export_vars`, `warn_missing_spec`, `warn_unused_import`
and `warnings_as_errors` are always on; every exported function must carry a
`-spec`. The `test` and `shell` profiles relax `warn_missing_spec` and
`warnings_as_errors`.

## Architecture

There is no supervision tree — yabko is a pure decoding library.

```
yabko          public API: decode/1, the object/0 type, format detection
├── yabko_bin  binary (bplist) decoder
└── yabko_xml  XML decoder (built on xmerl)
```

- `yabko:decode/1` sniffs the `bplist00`/`bplist01` magic to pick the binary
  decoder, otherwise falls back to the XML decoder; both are wrapped in a
  try/catch that turns any crash into `{error, {exception, …}}`.
- `yabko_bin` decodes the object table, resolves object-id references and
  reconstructs the tree, guarding against cycling references.
- `yabko_xml` walks the xmerl element tree; `CF$UID` single-key dicts decode
  to `{uid, _}`.

### Key modules

| Module | Role |
|---|---|
| `yabko` | Public API: `decode/1`; the `object/0`, `int64/0`, `uint64/0` types |
| `yabko_bin` | Binary `bplist` decoder (private; `-moduledoc false`) |
| `yabko_xml` | XML decoder built on `xmerl` (private; `-moduledoc false`) |

`src/yabko_common.hrl` carries the shared `nonempty_binary/0` fallback for
pre-OTP-24 (builtin on OTP 24+).

## Code conventions

- Code is formatted with `erlfmt`; run `make format` before committing. The
  bulk reformat commit is listed in `.git-blame-ignore-revs`.
- Docs are **EEP-48 native**: public `-moduledoc`/`-doc` attributes guarded by
  `-ifdef(E48).` (the `E48` macro is a `platform_define` for OTP 27+). Private
  modules and functions are hidden with `-moduledoc false` / `-doc false` —
  **not** the legacy `%% @private`, which ex_doc ignores.
- `make doc` runs `rebar3 edoc` (chunk doclet, configured in the top-level
  `edoc_opts`) then the ex_doc escript over `_build/docs/lib/yabko/ebin`. There
  is intentionally no `docs` rebar3 profile. The README is the docs main page.
- Documented `elvis.config` exceptions: lowercase `is_*` guard-style macros
  (`macro_naming_convention` regex) and xmerl's `xmlText` record tag matched as
  a literal atom (`atom_naming_convention` regex).
- Copyright lines read `Copyright (c) <years> Guilherme Andrade` (no email).

## Tests

There is no `test/` directory: the unit tests are inline `-ifdef(TEST)` eunit
functions in `yabko.erl`, each decoding a fixture from `test_data/` and
asserting the resulting term. `make test` also runs `rebar3 ct` (currently no
suites) so the house-style target stays consistent.

## Dependencies

Runtime: `iso8601 ~> 1.3` (date parsing in the XML decoder; the requirement is
loose because 1.3.3 fails to compile on OTP 27+, which reserves `maybe`) and
`xmerl` (from OTP).

Dev plugins: `erlfmt`, `rebar3_hank` (dead code), `rebar3_lint` (Elvis),
`rebar3_hex`. `erlfmt`/`rebar3_hank`/`rebar3_lint` are excluded on older OTP
versions via `rebar.config.script` (erlfmt on OTP ≤ 26 — `-doc` triple-quoted
strings break katana_code there; all three on ≤ 25; hank on OTP 29).

## OTP-version notes

The declared `minimum_otp_vsn` is low as a courtesy, but only **OTP 24+** is
actually supported and tested. EEP-48 doc attributes only compile on OTP 27+
(guarded by `E48`).

## Releasing

`make publish` runs `rebar3 hex publish --doc-dir=doc`. Versioning follows
SemVer; history is in `CHANGELOG.md` (Keep a Changelog format).

yabko is an independent project and has not been authorized, sponsored, or
otherwise approved by Apple Inc.
