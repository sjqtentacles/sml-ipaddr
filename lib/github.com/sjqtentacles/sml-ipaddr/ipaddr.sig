(* ipaddr.sig

   IPv4 / IPv6 address parsing, formatting, CIDR subnet arithmetic, and
   address classification, in pure Standard ML.

   Conventions
   -----------
   An IPv4 address is a 32-bit word (`Ipv4.t = Word32.word`). The octets are
   big-endian: the most significant byte of the word is the first dotted
   decimal octet. So `192.168.1.5` is `0wxC0A80105`.

   An IPv6 address is an ordered tuple of eight 16-bit pieces (`Ipv6.t`).
   The first piece is the leftmost hextet. Parsing accepts:

     - full eight-hextet form           `2001:0db8:0000:0000:0000:0000:0000:0001`
     - compressed `::` for one run of   `2001:db8::1`
       consecutive zero hextets (RFC 4291 §2.2)
     - IPv4-mapped / embedded form      `::ffff:192.0.2.1`  (lower 32 bits as dotted decimal)

   Formatting follows RFC 5952 canonical form:
     - lowercase hex, no leading zeros in a hextet
     - the *longest* run of all-zero hextets is compressed to `::`; ties broken
       by leftmost. A run of length 1 is NOT compressed.
     - if the address is IPv4-mapped (`::ffff:a.b.c.d`) or IPv4-compatible,
       the lower 32 bits are written in dotted decimal

   `IpAddr.t` is the sum type `V4 of Ipv4.t | V6 of Ipv6.t`. Its `fromString`
   dispatches on the presence of a colon (`:`) for v6 and a dot (`.`) for v4,
   falling back to the other if that fails. `toString` round-trips.

   CIDR
   ----
   A `Cidr.t` is `{addr : t, prefix : int}`. `network` masks `addr` to the
   network address; `broadcast` sets the host bits to 1; `contains` tests
   membership. The prefix length is in bits (0..32 for v4, 0..128 for v6).

   Classification
   --------------
   `Ipv4.Class.t` and `Ipv6.Class.t` are enumerated:

     - `Loopback`  : 127.0.0.0/8 (v4) ; ::1/128 (v6)
     - `Private`   : 10/8, 172.16/12, 192.168/16 (v4) ; fc00::/7 (v6, unique-local)
     - `LinkLocal` : 169.254/16 (v4) ; fe80::/10 (v6)
     - `Multicast` : 224/4 (v4) ; ff00::/8 (v6)
     - `Public`    : anything else *)

signature IPV4 =
sig
  type t = Word32.word

  exception Ipv4Error of string

  val fromString : string -> t option      (* dotted decimal *)
  val toString   : t -> string

  val fromBytes  : Word8.word list -> t option   (* MSB first, length 4 *)
  val toBytes    : t -> Word8.word list

  structure Cidr :
  sig
    type cidr = { addr : t, prefix : int }
    val fromString   : string -> cidr option    (* "a.b.c.d/n" *)
    val toString     : cidr -> string
    val network      : cidr -> t
    val broadcast    : cidr -> t
    val contains     : cidr -> t -> bool
    (* First and last host addresses (network+1 .. broadcast-1); for /31 and
       /32 the network and broadcast are returned as both bounds. *)
    val hosts        : cidr -> t * t
  end

  structure Class :
  sig
    datatype class = Loopback | Private | LinkLocal | Multicast | Public
    val classify : t -> class
    val toString : class -> string
  end
end

signature IPV6 =
sig
  (* Eight 16-bit hextets, leftmost first. *)
  type t = { h : Word16.word Vector.vector }

  exception Ipv6Error of string

  val fromString : string -> t option
  val toString   : t -> string                  (* RFC 5952 canonical *)

  val fromHextets : Word16.word list -> t option    (* exactly 8 *)
  val toHextets   : t -> Word16.word list

  structure Cidr :
  sig
    type cidr = { addr : t, prefix : int }
    val fromString : string -> cidr option
    val toString   : cidr -> string
    val network    : cidr -> t
    val broadcast  : cidr -> t
    val contains   : cidr -> t -> bool
  end

  structure Class :
  sig
    datatype class = Loopback | Private | LinkLocal | Multicast | Public
    val classify : t -> class
    val toString : class -> string
  end
end

signature IP_ADDR =
sig
  datatype t = V4 of Word32.word | V6 of { h : Word16.word Vector.vector }

  val fromString : string -> t option
  val toString   : t -> string
  val compare    : t * t -> order
end
