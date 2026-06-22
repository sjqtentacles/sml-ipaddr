# sml-ipaddr

[![CI](https://github.com/sjqtentacles/sml-ipaddr/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-ipaddr/actions/workflows/ci.yml)

IPv4 / IPv6 address parsing, formatting, CIDR subnet arithmetic, and address
classification for Standard ML. Pure, dependency-free, and deterministic across
compilers.

Part of the `sjqtentacles` monorepo of SML libraries.

## Features

- **IPv4**: dotted-decimal parse/format, byte conversion, CIDR
  (`network` / `broadcast` / `contains` / `hosts`), classification
  (loopback / private / link-local / multicast / public)
- **IPv6**: full, compressed (`::`), and IPv4-mapped (`::ffff:a.b.c.d`) parsing;
  RFC 5952 canonical formatting (lowercase hex, longest zero-run compressed);
  CIDR and classification analogous to IPv4
- **IpAddr**: sum type dispatching on `:` (v6) vs `.` (v4), with `compare`

## Status

Working, tested. No dependencies beyond the Basis library.

## Dependencies

None.

## Usage

```sml
val SOME v4 = Ipv4.fromString "192.168.1.5"
val s = Ipv4.toString v4                   (* "192.168.1.5" *)

val cidr = valOf (Ipv4.Cidr.fromString "192.168.0.0/16")
val inNet = Ipv4.Cidr.contains cidr v4     (* true *)

val SOME v6 = Ipv6.fromString "2001:0db8:0000:0000:0000:0000:0000:0001"
val canon = Ipv6.toString v6               (* "2001:db8::1" *)

val SOME a = IpAddr.fromString "::ffff:192.0.2.1"
val out = IpAddr.toString a                (* "::ffff:192.0.2.1" *)
```

## Building and testing

```sh
make test        # build + run the suite under MLton (default)
make test-poly   # run the suite under Poly/ML
make all-tests   # run under both
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-ipaddr
smlpkg sync
```

Then reference the library basis from your own `.mlb`:

```
lib/github.com/sjqtentacles/sml-ipaddr/sml-ipaddr.mlb
```

For Poly/ML, `use` the sources listed in `sources.mlb` in order.

## License

MIT. See [LICENSE](LICENSE).
