(* Tests for sml-ipaddr.

   Covers IPv4 / IPv6 parse+format round-trips, CIDR arithmetic (network /
   broadcast / contains), classification, IPv6 RFC 5952 canonical compression,
   and IpAddr dispatch. *)

structure IpAddrTests =
struct
  open Harness

  fun checkOpt (name, opt, expected) =
    case opt of
        SOME v => checkString name (expected, v)
      | NONE => checkBool name (true, false)

  fun checkNone (name, opt) =
    case opt of
        SOME _ => checkBool name (true, false)
      | NONE => checkBool name (true, true)

  fun run () =
    let
      val () = section "ipv4 parse / format round-trip"

      val () = checkOpt ("0.0.0.0",
                         Option.map Ipv4.toString (Ipv4.fromString "0.0.0.0"),
                         "0.0.0.0")
      val () = checkOpt ("255.255.255.255",
                         Option.map Ipv4.toString (Ipv4.fromString "255.255.255.255"),
                         "255.255.255.255")
      val () = checkOpt ("192.168.1.1",
                         Option.map Ipv4.toString (Ipv4.fromString "192.168.1.1"),
                         "192.168.1.1")
      val () = checkNone ("reject 256.0.0.1", Ipv4.fromString "256.0.0.1")
      val () = checkNone ("reject 1.2.3", Ipv4.fromString "1.2.3")
      val () = checkNone ("reject 1.2.3.4.5", Ipv4.fromString "1.2.3.4.5")

      val () = section "ipv4 fromBytes / toBytes"
      val () = checkBool "fromBytes [192,168,1,5]"
                        (true,
                         Ipv4.fromBytes [0w192, 0w168, 0w1, 0w5]
                         = Ipv4.fromString "192.168.1.5")
      val () = checkString "toBytes 10.0.0.1"
                           ("10,0,0,1",
                            String.concatWith ","
                              (List.map Int.toString
                                (List.map Word8.toInt
                                  (Ipv4.toBytes (valOf (Ipv4.fromString "10.0.0.1"))))))

      val () = section "ipv4 CIDR"
      val cidr16 = valOf (Ipv4.Cidr.fromString "192.168.0.0/16")
      val () = checkString "network 192.168.0.0/16"
                           ("192.168.0.0",
                            Ipv4.toString (Ipv4.Cidr.network cidr16))
      val () = checkString "broadcast 192.168.0.0/16"
                           ("192.168.255.255",
                            Ipv4.toString (Ipv4.Cidr.broadcast cidr16))
      val () = checkBool "contains 192.168.5.5"
                        (true, Ipv4.Cidr.contains cidr16 (valOf (Ipv4.fromString "192.168.5.5")))
      val () = checkBool "not contains 10.0.0.1"
                        (false, Ipv4.Cidr.contains cidr16 (valOf (Ipv4.fromString "10.0.0.1")))
      val cidr24 = valOf (Ipv4.Cidr.fromString "192.168.1.0/24")
      val () = checkString "network 192.168.1.0/24"
                           ("192.168.1.0",
                            Ipv4.toString (Ipv4.Cidr.network cidr24))
      val () = checkString "broadcast 192.168.1.0/24"
                           ("192.168.1.255",
                            Ipv4.toString (Ipv4.Cidr.broadcast cidr24))
      val () = checkString "cidr toString" ("192.168.1.0/24",
                                            Ipv4.Cidr.toString cidr24)

      val () = section "ipv4 classification"
      val () = checkString "127.0.0.1 loopback"
                           ("loopback",
                            Ipv4.Class.toString
                              (Ipv4.Class.classify (valOf (Ipv4.fromString "127.0.0.1"))))
      val () = checkString "10.0.0.1 private"
                           ("private",
                            Ipv4.Class.toString
                              (Ipv4.Class.classify (valOf (Ipv4.fromString "10.0.0.1"))))
      val () = checkString "172.16.0.1 private"
                           ("private",
                            Ipv4.Class.toString
                              (Ipv4.Class.classify (valOf (Ipv4.fromString "172.16.0.1"))))
      val () = checkString "172.31.255.255 private"
                           ("private",
                            Ipv4.Class.toString
                              (Ipv4.Class.classify (valOf (Ipv4.fromString "172.31.255.255"))))
      val () = checkString "192.168.0.1 private"
                           ("private",
                            Ipv4.Class.toString
                              (Ipv4.Class.classify (valOf (Ipv4.fromString "192.168.0.1"))))
      val () = checkString "169.254.0.1 link-local"
                           ("link-local",
                            Ipv4.Class.toString
                              (Ipv4.Class.classify (valOf (Ipv4.fromString "169.254.0.1"))))
      val () = checkString "224.0.0.1 multicast"
                           ("multicast",
                            Ipv4.Class.toString
                              (Ipv4.Class.classify (valOf (Ipv4.fromString "224.0.0.1"))))
      val () = checkString "8.8.8.8 public"
                           ("public",
                            Ipv4.Class.toString
                              (Ipv4.Class.classify (valOf (Ipv4.fromString "8.8.8.8"))))

      val () = section "ipv6 parse / format"

      val () = checkOpt ("::1 round-trip",
                         Option.map Ipv6.toString (Ipv6.fromString "::1"),
                         "::1")
      val () = checkOpt ("2001:db8::1 round-trip",
                         Option.map Ipv6.toString (Ipv6.fromString "2001:db8::1"),
                         "2001:db8::1")
      val () = checkOpt (":: round-trip",
                         Option.map Ipv6.toString (Ipv6.fromString "::"),
                         "::")
      val () = checkOpt ("full 2001:0db8:0000:0000:0000:0000:0000:0001 canonical",
                         Option.map Ipv6.toString
                           (Ipv6.fromString "2001:0db8:0000:0000:0000:0000:0000:0001"),
                         "2001:db8::1")
      val () = checkOpt ("2001:db8:0:0:0:0:0:1 canonical",
                         Option.map Ipv6.toString
                           (Ipv6.fromString "2001:db8:0:0:0:0:0:1"),
                         "2001:db8::1")
      (* RFC 5952 §5.3: longest run wins; ties -> leftmost. Here the right
         run (3 zeros) is longer than the left (2 zeros), so the right is
         compressed. *)
      val () = checkOpt ("2001:0:0:1:0:0:0:1 longest-run compressed",
                         Option.map Ipv6.toString
                           (Ipv6.fromString "2001:0:0:1:0:0:0:1"),
                         "2001:0:0:1::1")
      (* Two runs of equal length (3 each) -> leftmost is compressed. *)
      val () = checkOpt ("0:0:0:1:0:0:0:1 leftmost of equal runs",
                         Option.map Ipv6.toString
                           (Ipv6.fromString "0:0:0:1:0:0:0:1"),
                         "::1:0:0:0:1")
      (* A single zero hextet is NOT compressed. *)
      val () = checkOpt ("2001:db8:0:1:2:3:4:5 no single-zero compression",
                         Option.map Ipv6.toString
                           (Ipv6.fromString "2001:db8:0:1:2:3:4:5"),
                         "2001:db8:0:1:2:3:4:5")

      val () = section "ipv6 v4-mapped"
      val () = checkOpt ("::ffff:192.0.2.1 round-trip",
                         Option.map Ipv6.toString
                           (Ipv6.fromString "::ffff:192.0.2.1"),
                         "::ffff:192.0.2.1")
      val () = checkNone ("reject 1:2:3:4:5:6:7",
                          Ipv6.fromString "1:2:3:4:5:6:7")
      val () = checkNone ("reject 1:2:3:4:5:6:7:8:9",
                          Ipv6.fromString "1:2:3:4:5:6:7:8:9")

      val () = section "ipv6 CIDR"
      val cidr6 = valOf (Ipv6.Cidr.fromString "2001:db8::/32")
      val () = checkString "cidr6 toString" ("2001:db8::/32",
                                             Ipv6.Cidr.toString cidr6)
      val () = checkString "network 2001:db8::/32"
                           ("2001:db8::",
                            Ipv6.toString (Ipv6.Cidr.network cidr6))
      val () = checkBool "contains 2001:db8:1::1"
                        (true, Ipv6.Cidr.contains cidr6
                                 (valOf (Ipv6.fromString "2001:db8:1::1")))
      val () = checkBool "not contains 2001:db9::1"
                        (false, Ipv6.Cidr.contains cidr6
                                  (valOf (Ipv6.fromString "2001:db9::1")))

      val () = section "ipv6 classification"
      val () = checkString "::1 loopback"
                           ("loopback",
                            Ipv6.Class.toString
                              (Ipv6.Class.classify (valOf (Ipv6.fromString "::1"))))
      val () = checkString "ff00::1 multicast"
                           ("multicast",
                            Ipv6.Class.toString
                              (Ipv6.Class.classify (valOf (Ipv6.fromString "ff00::1"))))
      val () = checkString "fc00::1 private"
                           ("private",
                            Ipv6.Class.toString
                              (Ipv6.Class.classify (valOf (Ipv6.fromString "fc00::1"))))
      val () = checkString "fd00::1 private"
                           ("private",
                            Ipv6.Class.toString
                              (Ipv6.Class.classify (valOf (Ipv6.fromString "fd00::1"))))
      val () = checkString "fe80::1 link-local"
                           ("link-local",
                            Ipv6.Class.toString
                              (Ipv6.Class.classify (valOf (Ipv6.fromString "fe80::1"))))
      val () = checkString "2001:db8::1 public"
                           ("public",
                            Ipv6.Class.toString
                              (Ipv6.Class.classify (valOf (Ipv6.fromString "2001:db8::1"))))

      val () = section "IpAddr dispatch"
      val () = checkString "v4 dispatch" ("192.168.1.1",
        case IpAddr.fromString "192.168.1.1" of
            SOME a => IpAddr.toString a | NONE => "NONE")
      val () = checkString "v6 dispatch" ("2001:db8::1",
        case IpAddr.fromString "2001:db8::1" of
            SOME a => IpAddr.toString a | NONE => "NONE")
      val () = checkString "v6 ::1 dispatch" ("::1",
        case IpAddr.fromString "::1" of
            SOME a => IpAddr.toString a | NONE => "NONE")
      val () = checkString "v6 v4-mapped dispatch" ("::ffff:192.0.2.1",
        case IpAddr.fromString "::ffff:192.0.2.1" of
            SOME a => IpAddr.toString a | NONE => "NONE")
      val () = checkBool "compare v4 < v6"
                        (true,
                         IpAddr.compare (IpAddr.V4 0w0, IpAddr.V6 (valOf (Ipv6.fromString "::1")))
                         = LESS)
      val () = checkBool "compare equal v4"
                        (true,
                         IpAddr.compare (IpAddr.V4 0w1, IpAddr.V4 0w1) = EQUAL)

      (* Oversized untrusted numeric fields (IPv4 octet, IPv4/IPv6 CIDR prefix)
         must be rejected with NONE, never raise Overflow. On this toolchain
         MLton's Int is 32-bit and Poly/ML's is 63-bit (both fixed width; only
         IntInf is arbitrary), so an unchecked Int.fromString would crash on
         MLton for a value past 2^31 and diverge from Poly/ML. Inputs sit just
         past 2^31 (2147483648) and at 12 digits so both compilers must agree. *)
      val () = section "integer overflow (untrusted numeric input)"
      (* A raise surfaces as a bare non-NONE sentinel so a crash is a clean FAIL
         rather than aborting the binary. *)
      fun raised f x = (ignore (f x); false) handle Overflow => true | _ => false
      fun rejects f x = (case f x of SOME _ => false | NONE => true)
                        handle _ => false
      (* IPv4 octet just over 2^31 and 12 digits. *)
      val () = checkBool "ipv4 octet 2147483648 -> NONE (no raise)"
                 (true, rejects Ipv4.fromString "2147483648.0.0.1")
      val () = checkBool "ipv4 octet 12 digits -> NONE (no raise)"
                 (true, rejects Ipv4.fromString "999999999999.0.0.1")
      val () = checkBool "ipv4 octet does not raise Overflow"
                 (false, raised Ipv4.fromString "999999999999.0.0.1")
      (* IPv4 CIDR prefix (line 92). *)
      val () = checkBool "ipv4 CIDR prefix 2147483648 -> NONE (no raise)"
                 (true, rejects Ipv4.Cidr.fromString "10.0.0.0/2147483648")
      val () = checkBool "ipv4 CIDR prefix 12 digits -> NONE (no raise)"
                 (true, rejects Ipv4.Cidr.fromString "10.0.0.0/999999999999")
      val () = checkBool "ipv4 CIDR prefix does not raise Overflow"
                 (false, raised Ipv4.Cidr.fromString "10.0.0.0/999999999999")
      (* IPv6 CIDR prefix (line 363). *)
      val () = checkBool "ipv6 CIDR prefix 2147483648 -> NONE (no raise)"
                 (true, rejects Ipv6.Cidr.fromString "2001:db8::/2147483648")
      val () = checkBool "ipv6 CIDR prefix 12 digits -> NONE (no raise)"
                 (true, rejects Ipv6.Cidr.fromString "2001:db8::/999999999999")
      val () = checkBool "ipv6 CIDR prefix does not raise Overflow"
                 (false, raised Ipv6.Cidr.fromString "2001:db8::/999999999999")
      (* Normal, in-range values still parse. *)
      val () = checkBool "normal ipv4 octet still parses"
                 (true, isSome (Ipv4.fromString "192.168.1.1"))
      val () = checkBool "normal ipv4 CIDR still parses"
                 (true, isSome (Ipv4.Cidr.fromString "10.0.0.0/8"))
      val () = checkBool "normal ipv6 CIDR still parses"
                 (true, isSome (Ipv6.Cidr.fromString "2001:db8::/32"))
    in
      ()
    end
end
