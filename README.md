# yabko

**This library is not under active maintenance; if you'd like to perform
maintenance yourself, feel free to open an issue requesting access.**

[![](https://img.shields.io/hexpm/v/yabko.svg?style=flat)](https://hex.pm/packages/yabko)
[![](https://github.com/g-andrade/yabko/workflows/build/badge.svg)](https://github.com/g-andrade/yabko/actions?query=workflow%3Abuild)

`yabko` is an Erlang parser of Apple Property Lists (.plist)

### Decodable data types

-   boolean
-   null (binary format)
-   integer
-   real
-   date
-   uid
-   binary data
-   latin1 text (binary format)
-   utf16 text (binary format)
-   utf8 text (XML format)
-   arrays
-   sets (binary format; untested)
-   dictionaries

### Example Usage

    $ make console

``` erlang
% 1> {ok, XmlEncoded} = file:read_file("test_data/doc_example.xml").
{ok,<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE plist "...>>}

% 2> yabko:decode(XmlEncoded).
{ok,#{<<"Codswallop ratio">> => 3.14159265359,
      <<"Date of birth">> => {{1973,2,12},{9,18,0}},
      <<"Name">> => <<"John Doé"/utf8>>,
      <<"Owned foobars">> => 4321,
      <<"Pocket contents">> =>
          [42,<<"Lorem ipsum">>,{uid,123456}]}}

% 3> {ok, BinEncoded} = file:read_file("test_data/float32.bin.plist").
{ok,<<98,112,108,105,115,116,48,48,209,1,2,89,101,116,99,
      32,101,116,99,46,46,34,63,128,0,0,8,...>>}

% 4> yabko:decode(BinEncoded)
{ok,#{<<"etc etc..">> => 1.0}}
```

Yabko is an independent project and has not been authorized, sponsored,
or otherwise approved by Apple Inc.
