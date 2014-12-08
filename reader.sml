val test = TextIO.openIn "test.core" (* "std-lib.core" *)

fun isTerminator c = (Char.isSpace c) orelse (#")" = c)

fun isnt f = fn arg => not (f arg)

fun scan_symbol reader s = 
    case StringCvt.splitl (isnt isTerminator) reader s of
	("", ns) => NONE
      | (w, ns) => SOME (w, ns)

fun scan_word reader state = let
    val s = StringCvt.skipWS reader state
in case StringCvt.splitl Char.isAlpha reader s of
       ("", ns) => NONE
     | (w, ns) => SOME (w, ns)
end

fun scan stream = TextIO.scanStream scan_symbol stream
