(* demo.sml - IPv4/IPv6 parsing, RFC 5952 canonical formatting, CIDR subnet
   arithmetic, classification, and IpAddr dispatch, all over in-memory
   literals: no network I/O anywhere. Deterministic: identical output under
   MLton and Poly/ML. *)

structure V4 = Ipv4
structure V6 = Ipv6

val () = print "IPv4 parse / format / classify:\n"
val v4 = valOf (V4.fromString "192.168.1.5")
val () = print ("  192.168.1.5 -> " ^ V4.toString v4
                ^ " (" ^ V4.Class.toString (V4.Class.classify v4) ^ ")\n")
val pub4 = valOf (V4.fromString "8.8.8.8")
val () = print ("  8.8.8.8     -> " ^ V4.toString pub4
                ^ " (" ^ V4.Class.toString (V4.Class.classify pub4) ^ ")\n")

val () = print "\nIPv4 CIDR (192.168.1.0/24):\n"
val cidr4 = valOf (V4.Cidr.fromString "192.168.1.0/24")
val () = print ("  network   = " ^ V4.toString (V4.Cidr.network cidr4) ^ "\n")
val () = print ("  broadcast = " ^ V4.toString (V4.Cidr.broadcast cidr4) ^ "\n")
val () = print ("  contains 192.168.1.200? "
                ^ Bool.toString (V4.Cidr.contains cidr4 (valOf (V4.fromString "192.168.1.200"))) ^ "\n")

val () = print "\nIPv6 parse / RFC 5952 canonical format:\n"
val v6 = valOf (V6.fromString "2001:0db8:0000:0000:0000:0000:0000:0001")
val () = print ("  2001:0db8:0000:...:0001 -> " ^ V6.toString v6 ^ "\n")
val mapped = valOf (V6.fromString "::ffff:192.0.2.1")
val () = print ("  ::ffff:192.0.2.1        -> " ^ V6.toString mapped ^ "\n")
val () = print ("  class of ::1 = " ^ V6.Class.toString (V6.Class.classify (valOf (V6.fromString "::1"))) ^ "\n")

val () = print "\nIPv6 CIDR (2001:db8::/32):\n"
val cidr6 = valOf (V6.Cidr.fromString "2001:db8::/32")
val () = print ("  network = " ^ V6.toString (V6.Cidr.network cidr6) ^ "\n")
val () = print ("  contains 2001:db8:1::1? "
                ^ Bool.toString (V6.Cidr.contains cidr6 (valOf (V6.fromString "2001:db8:1::1"))) ^ "\n")

val () = print "\nIpAddr dispatch + compare:\n"
val a = valOf (IpAddr.fromString "192.168.1.1")
val b = valOf (IpAddr.fromString "2001:db8::1")
val () = print ("  " ^ IpAddr.toString a ^ " vs " ^ IpAddr.toString b ^ " -> "
                ^ (case IpAddr.compare (a, b) of LESS => "LESS" | EQUAL => "EQUAL" | GREATER => "GREATER")
                ^ "\n")
