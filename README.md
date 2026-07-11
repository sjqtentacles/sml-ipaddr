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

Working, tested (62 deterministic checks, byte-identical under MLton and
Poly/ML). No dependencies beyond the Basis library.

Numeric fields parsed from untrusted strings (IPv4 octets, IPv4/IPv6 CIDR
prefix lengths) are range-checked via `IntInf` and bounded to a fixed 32-bit
signed range, so an oversized value returns `NONE` instead of raising
`Overflow`. This matters because on this toolchain MLton's `Int` is 32-bit and
Poly/ML's is 63-bit (both fixed width; only `IntInf` is arbitrary precision),
so an unchecked parse would crash on MLton and diverge from Poly/ML.

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

## Example

`make example` builds and runs [`examples/demo.sml`](examples/demo.sml), which
parses and formats IPv4/IPv6 addresses, computes CIDR network/broadcast/
containment, classifies addresses, and dispatches through `IpAddr` — all over
in-memory literals, no network I/O (output is byte-identical under MLton and
Poly/ML):

```
IPv4 parse / format / classify:
  192.168.1.5 -> 192.168.1.5 (private)
  8.8.8.8     -> 8.8.8.8 (public)

IPv4 CIDR (192.168.1.0/24):
  network   = 192.168.1.0
  broadcast = 192.168.1.255
  contains 192.168.1.200? true

IPv6 parse / RFC 5952 canonical format:
  2001:0db8:0000:...:0001 -> 2001:db8::1
  ::ffff:192.0.2.1        -> ::ffff:192.0.2.1
  class of ::1 = loopback

IPv6 CIDR (2001:db8::/32):
  network = 2001:db8::
  contains 2001:db8:1::1? true

IpAddr dispatch + compare:
  192.168.1.1 vs 2001:db8::1 -> LESS
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
