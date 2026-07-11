(* ipaddr.sml

   Implementation of IPv4 / IPv6 / IpAddr. See ipaddr.sig for the conventions.

   Parsing strategy
   ----------------
   IPv4: split on `.` into exactly four pieces, each parsed as a decimal
   integer 0..255. Combine big-endian into a Word32.word.

   IPv6: at most one `::`. Split around it into a left half and a right half
   (either may be empty). The right half may end with an IPv4 dotted quad
   (occupying the last two hextets). Parse hextets in both halves; fill the
   middle with zeros so the total is 8.

   Formatting (RFC 5952): the longest run of all-zero hextets (length >= 2) is
   compressed to `::`, ties broken leftmost. IPv4-mapped addresses
   (`::ffff:a.b.c.d`) emit the lower 32 bits as dotted decimal. *)

(* Parse an int from untrusted input without ever raising Overflow. On this
   toolchain MLton's Int is 32-bit and Poly/ML's is 63-bit (both fixed width;
   only IntInf is arbitrary), so a plain Int.fromString raises Overflow on
   MLton for a value past 2^31 while Poly/ML would accept it: a crash and a
   cross-compiler divergence. Parse via IntInf and bound to a fixed 32-bit
   signed range so both compilers behave identically; callers apply their own
   tighter range check (octet 0..255, prefix 0..32 / 0..128) afterward. *)
fun parseIntBounded s =
  case IntInf.fromString s of
      SOME n => if n >= ~2147483648 andalso n <= 2147483647
                then SOME (IntInf.toInt n) else NONE
    | NONE => NONE

structure Ipv4 : IPV4 =
struct
  type t = Word32.word

  exception Ipv4Error of string

  (* Word32 has no `not` in the Basis; define a local helper. *)
  fun notw32 w = Word32.xorb (w, 0wxFFFFFFFF)

  fun fromString s =
    let
      val parts = String.fields (fn c => c = #".") s
      fun parseByte p =
        case parseIntBounded p of
            SOME n => if n >= 0 andalso n <= 255 then SOME (Word8.fromInt n)
                      else NONE
          | NONE => NONE
    in
      case parts of
          [a, b, c, d] =>
            (case (parseByte a, parseByte b, parseByte c, parseByte d) of
                 (SOME wa, SOME wb, SOME wc, SOME wd) =>
                   let
                     val w = Word32.fromLarge (Word8.toLarge wa) * 0wx1000000
                           + Word32.fromLarge (Word8.toLarge wb) * 0wx10000
                           + Word32.fromLarge (Word8.toLarge wc) * 0wx100
                           + Word32.fromLarge (Word8.toLarge wd)
                   in SOME w end
               | _ => NONE)
        | _ => NONE
    end

  fun toBytes w =
    let
      val b0 = Word8.fromLarge (Word32.toLarge (Word32.>> (w, 0w24)))
      val b1 = Word8.fromLarge (Word32.toLarge (Word32.>> (w, 0w16)))
      val b2 = Word8.fromLarge (Word32.toLarge (Word32.>> (w, 0w8)))
      val b3 = Word8.fromLarge (Word32.toLarge w)
    in [b0, b1, b2, b3] end

  fun fromBytes bs =
    case bs of
        [b0, b1, b2, b3] =>
          let
            val w = Word32.fromLarge (Word8.toLarge b0) * 0wx1000000
                  + Word32.fromLarge (Word8.toLarge b1) * 0wx10000
                  + Word32.fromLarge (Word8.toLarge b2) * 0wx100
                  + Word32.fromLarge (Word8.toLarge b3)
          in SOME w end
      | _ => NONE

  fun toString w =
    case toBytes w of
        [b0, b1, b2, b3] =>
          String.concatWith "."
            [ Int.toString (Word8.toInt b0)
            , Int.toString (Word8.toInt b1)
            , Int.toString (Word8.toInt b2)
            , Int.toString (Word8.toInt b3) ]
      | _ => raise Fail "Ipv4.toString: impossible"

  (* Capture outer bindings before substructures shadow them. *)
  val outerFromString = fromString
  val outerToString = toString

  structure Cidr =
  struct
    type cidr = { addr : t, prefix : int }

    fun fromString s =
      case String.fields (fn c => c = #"/") s of
          [addrStr, pfxStr] =>
            (case (outerFromString addrStr, parseIntBounded pfxStr) of
                 (SOME a, SOME p) =>
                   if p >= 0 andalso p <= 32 then SOME { addr = a, prefix = p }
                   else NONE
               | _ => NONE)
        | _ => NONE

    fun toString ({ addr = a, prefix = p } : cidr) =
      outerToString a ^ "/" ^ Int.toString p

    (* A mask of `prefix` leading 1-bits, big-endian. prefix in [0,32]. *)
    fun mkMask prefix =
      if prefix = 0 then 0w0
      else if prefix >= 32 then 0wxFFFFFFFF
      else notw32 (Word32.>> (0wxFFFFFFFF, Word.fromInt prefix))

    fun network { addr = a, prefix = p } = Word32.andb (a, mkMask p)
    fun broadcast { addr = a, prefix = p } = Word32.orb (a, notw32 (mkMask p))

    fun contains ({ addr = a, prefix = p } : cidr) x =
      Word32.andb (x, mkMask p) = Word32.andb (a, mkMask p)

    fun hosts { addr = a, prefix = p } =
      let
        val net = Word32.andb (a, mkMask p)
        val bcast = Word32.orb (a, notw32 (mkMask p))
      in
        if p >= 31 then (net, bcast)
        else (net + 0w1, bcast - 0w1)
      end
  end

  structure Class =
  struct
    datatype class = Loopback | Private | LinkLocal | Multicast | Public

    fun classify w =
      let
        val b0 = Word8.toInt (Word8.fromLarge (Word32.toLarge (Word32.>> (w, 0w24))))
        val b1 = Word8.toInt (Word8.fromLarge (Word32.toLarge (Word32.>> (w, 0w16))))
      in
        if b0 = 127 then Loopback
        else if b0 = 10 orelse
                (b0 = 172 andalso b1 >= 16 andalso b1 <= 31) orelse
                (b0 = 192 andalso b1 = 168) then Private
        else if b0 = 169 andalso b1 = 254 then LinkLocal
        else if b0 >= 224 andalso b0 <= 239 then Multicast
        else Public
      end

    fun toString cl =
      case cl of
          Loopback => "loopback"
        | Private => "private"
        | LinkLocal => "link-local"
        | Multicast => "multicast"
        | Public => "public"
  end
end

structure Ipv6 : IPV6 =
struct
  type t = { h : Word16.word Vector.vector }

  exception Ipv6Error of string

  val zeroHextet : Word16.word = 0w0

  fun fromHextets (hs : Word16.word list) =
    case hs of
        [h0,h1,h2,h3,h4,h5,h6,h7] =>
          SOME { h = Vector.fromList hs }
      | _ => NONE

  fun toHextets ({ h = hv } : t) = Vector.foldr (op ::) [] hv

  fun parseHextet s =
    if s = "" orelse String.size s > 4 then NONE
    else
      let
        val chars = explode s
        fun isHex c =
          (#"0" <= c andalso c <= #"9") orelse
          (#"a" <= c andalso c <= #"f") orelse
          (#"A" <= c andalso c <= #"F")
      in
        if List.all isHex chars then
          (case StringCvt.scanString (Word16.scan StringCvt.HEX) s of
               SOME w => SOME w
             | NONE => NONE)
        else NONE
      end

  (* Parse an IPv4 dotted quad, returning the two hextets (high, low). *)
  fun parseV4Tail s =
    case Ipv4.fromString s of
        SOME w =>
          let
            val hi = Word16.fromLarge (Word32.toLarge (Word32.>> (w, 0w16)))
            val lo = Word16.fromLarge (Word32.toLarge (Word32.andb (w, 0wxFFFF)))
          in SOME (hi, lo) end
      | NONE => NONE

  (* Parse a colon-delimited segment list, recognising a trailing IPv4 quad
     (only allowed as the final segment). Returns the hextets in order. *)
  fun parseSegments (segs : string list) : Word16.word list option =
    case segs of
        [] => SOME []
      | [seg] =>
          if String.isSubstring "." seg then
            (case parseV4Tail seg of
                 SOME (hi, lo) => SOME [hi, lo]
               | NONE => NONE)
          else
            (case parseHextet seg of SOME w => SOME [w] | NONE => NONE)
      | seg :: rest =>
          if String.isSubstring "." seg then NONE  (* v4 only at end *)
          else
            (case parseHextet seg of
                 SOME w =>
                   (case parseSegments rest of
                        SOME ws => SOME (w :: ws)
                      | NONE => NONE)
               | NONE => NONE)

  fun findDoubleColon s =
    let
      val n = String.size s
      fun go i =
        if i + 1 >= n then NONE
        else if String.sub (s, i) = #":" andalso String.sub (s, i + 1) = #":"
        then SOME i
        else go (i + 1)
    in go 0 end

  fun fromString s =
    case findDoubleColon s of
        NONE =>
          let val segs = String.fields (fn c => c = #":") s
          in case parseSegments segs of
                 SOME ws => if List.length ws = 8 then fromHextets ws else NONE
               | NONE => NONE
          end
      | SOME idx =>
          let
            val leftStr = String.substring (s, 0, idx)
            val rightStr = String.extract (s, idx + 2, NONE)
            val leftSegs =
              if leftStr = "" then [] else String.fields (fn c => c = #":") leftStr
            val rightSegs =
              if rightStr = "" then [] else String.fields (fn c => c = #":") rightStr
          in
            case (parseSegments leftSegs, parseSegments rightSegs) of
                (SOME lhs, SOME rhs) =>
                  let
                    val lhsN = List.length lhs
                    val rhsN = List.length rhs
                  in
                    if lhsN + rhsN > 8 then NONE
                    else
                      let
                        val zerosNeeded = 8 - lhsN - rhsN
                        val zeros = List.tabulate (zerosNeeded, fn _ => zeroHextet)
                      in fromHextets (lhs @ zeros @ rhs) end
                  end
              | _ => NONE
          end

  (* ---- RFC 5952 canonical formatting ---- *)

  (* Find the longest run of zero hextets in the first `count` slots.
     Returns SOME (start, length) for the longest run with length >= 2,
     leftmost on ties. *)
  fun longestZeroRun (hs, count) : (int * int) option =
    let
      fun scan (i, curStart, curLen, best) =
        if i >= count then
          let val finalCur = if curLen >= 2 then SOME (curStart, curLen) else NONE
          in
            case (best, finalCur) of
                (SOME (bs, bl), SOME (_, cl)) =>
                  if cl > bl then finalCur else best
              | (SOME _, NONE) => best
              | (NONE, _) => finalCur
          end
        else if Vector.sub (hs, i) = zeroHextet then
          let val cs = if curLen = 0 then i else curStart
          in scan (i + 1, cs, curLen + 1, best) end
        else
          let
            val newBest =
              case best of
                  SOME (bs, bl) =>
                    if curLen > bl then SOME (curStart, curLen) else best
                | NONE => if curLen >= 2 then SOME (curStart, curLen) else best
          in scan (i + 1, 0, 0, newBest) end
    in scan (0, 0, 0, NONE) end

  fun hextetStr w =
    String.map Char.toLower (Word16.toString w)

  fun toString ({ h = hv } : t) =
    let
      val useV4 =
        Vector.length hv = 8 andalso
        Vector.sub (hv, 0) = 0w0 andalso
        Vector.sub (hv, 1) = 0w0 andalso
        Vector.sub (hv, 2) = 0w0 andalso
        Vector.sub (hv, 3) = 0w0 andalso
        Vector.sub (hv, 4) = 0w0 andalso
        Vector.sub (hv, 5) = 0wxFFFF
      val nHextets = if useV4 then 6 else 8
      val zeroRun = longestZeroRun (hv, nHextets)
      fun piece i = hextetStr (Vector.sub (hv, i))
      val body =
        case zeroRun of
            NONE =>
              String.concatWith ":" (List.tabulate (nHextets, piece))
          | SOME (start, len) =>
              let
                val prefix = List.tabulate (start, piece)
                val suffix = List.tabulate (nHextets - (start + len),
                              fn i => piece (start + len + i))
                val prefixStr = String.concatWith ":" prefix
                val suffixStr = String.concatWith ":" suffix
              in
                if start = 0 andalso start + len = nHextets then "::"
                else if start = 0 then "::" ^ suffixStr
                else if start + len = nHextets then prefixStr ^ "::"
                else prefixStr ^ "::" ^ suffixStr
              end
    in
      if useV4 then
        let
          val hi = Vector.sub (hv, 6)
          val lo = Vector.sub (hv, 7)
          val w32 = Word32.fromLarge (Word16.toLarge hi) * 0wx10000
                  + Word32.fromLarge (Word16.toLarge lo)
        in
          body ^ ":" ^ Ipv4.toString w32
        end
      else body
    end

  (* Capture outer bindings before substructures shadow them. *)
  val outerFromString = fromString
  val outerToString = toString

  structure Cidr =
  struct
    type cidr = { addr : t, prefix : int }

    (* Bit-level mask of `prefix` leading ones, as 8 hextets. *)
    fun masks prefix =
      let
        fun hextetMask i =
          let val startBit = i * 16
              val endBit = startBit + 16
          in
            if prefix <= startBit then 0w0
            else if prefix >= endBit then 0wxFFFF
            else
              let val bits = prefix - startBit
                  val shift = 16 - bits
              in Word16.<< (0wxFFFF, Word.fromInt shift) end
          end
      in Vector.tabulate (8, hextetMask) end

    fun fromString s =
      case String.fields (fn c => c = #"/") s of
          [addrStr, pfxStr] =>
            (case (outerFromString addrStr, parseIntBounded pfxStr) of
                 (SOME a, SOME p) =>
                   if p >= 0 andalso p <= 128 then SOME { addr = a, prefix = p }
                   else NONE
               | _ => NONE)
        | _ => NONE

    fun toString ({ addr = a, prefix = p } : cidr) =
      outerToString a ^ "/" ^ Int.toString p

    fun network { addr = a, prefix = p } =
      let val m = masks p
          val { h = h } = a
          val h' = Vector.tabulate
                     (8, fn i => Word16.andb (Vector.sub (h, i), Vector.sub (m, i)))
      in { h = h' } end

    fun broadcast { addr = a, prefix = p } =
      let val m = masks p
          val { h = h } = a
          fun notw16 w = Word16.xorb (w, 0wxFFFF)
          val h' = Vector.tabulate
                     (8, fn i => Word16.orb (Vector.sub (h, i),
                                              notw16 (Vector.sub (m, i))))
      in { h = h' } end

    fun contains ({ addr = a, prefix = p } : cidr) (x : t) =
      let
        val m = masks p
        val ah = #h a
        val xh = #h x
        fun all i =
          i >= 8 orelse
          (Word16.andb (Vector.sub (xh, i), Vector.sub (m, i)) =
           Word16.andb (Vector.sub (ah, i), Vector.sub (m, i))
           andalso all (i + 1))
      in all 0 end
  end

  structure Class =
  struct
    datatype class = Loopback | Private | LinkLocal | Multicast | Public

    fun classify ({ h = hv } : t) =
      let
        val h0 = Vector.sub (hv, 0)
      in
        if Vector.length hv = 8 andalso
           Vector.sub (hv, 0) = 0w0 andalso
           Vector.sub (hv, 1) = 0w0 andalso
           Vector.sub (hv, 2) = 0w0 andalso
           Vector.sub (hv, 3) = 0w0 andalso
           Vector.sub (hv, 4) = 0w0 andalso
           Vector.sub (hv, 5) = 0w0 andalso
           Vector.sub (hv, 6) = 0w0 andalso
           Vector.sub (hv, 7) = 0w1 then Loopback
        else if Word16.>> (h0, 0w8) = 0wxFF then Multicast       (* ff00::/8 *)
        else if Word16.>> (h0, 0w9) = 0wx7E then Private          (* fc00::/7 *)
        else if Word16.>> (h0, 0w6) = 0wx3FA then LinkLocal       (* fe80::/10 *)
        else Public
      end

    fun toString cl =
      case cl of
          Loopback => "loopback"
        | Private => "private"
        | LinkLocal => "link-local"
        | Multicast => "multicast"
        | Public => "public"
  end
end

structure IpAddr : IP_ADDR =
struct
  datatype t = V4 of Ipv4.t | V6 of Ipv6.t

  fun fromString s =
    if String.isSubstring ":" s then
      (case Ipv6.fromString s of
           SOME v6 => SOME (V6 v6)
         | NONE => NONE)
    else
      (case Ipv4.fromString s of
           SOME v4 => SOME (V4 v4)
         | NONE => NONE)

  fun toString (V4 v4) = Ipv4.toString v4
    | toString (V6 v6) = Ipv6.toString v6

  fun compare (V4 a, V4 b) = Word32.compare (a, b)
    | compare (V4 _, V6 _) = LESS
    | compare (V6 _, V4 _) = GREATER
    | compare (V6 a, V6 b) =
        let
          val ha = #h a
          val hb = #h b
          fun go i =
            if i >= 8 then EQUAL
            else
              case Word16.compare (Vector.sub (ha, i), Vector.sub (hb, i)) of
                  EQUAL => go (i + 1)
                | ord => ord
        in go 0 end
end
