val test = TextIO.openIn "test.core" (* "std-lib.core" *)

datatype LispVal = Cell of LispVal * LispVal
		 | Symbol of string
		 | String of string
		 | Number of int
		 | Nil

fun isTerminator c = (Char.isSpace c) orelse (#")" = c)
fun isQuote c = #"\"" = c

fun isnt f = fn arg => not (f arg)

fun scan_symbol reader s =
    case StringCvt.splitl (isnt isTerminator) reader s of
	("", ns) => NONE
      | (w, ns) => SOME (Symbol w, ns)

fun scan_integer reader s =
    case StringCvt.splitl Char.isDigit reader s of
	("", ns) => NONE
     |  (w, ns) => case Int.fromString w of
		       NONE => NONE
		    | SOME n => SOME (Number n, ns)

fun scan_string reader s =
    case StringCvt.splitl (isnt isQuote) reader s of
	("", ns) => NONE
      | (w, ns) => SOME (String w, ns)

fun scan_dashed reader s =
    case reader s of
	NONE => NONE
      | SOME(c, ns) => if Char.isDigit c
		       then scan_integer reader ns
		       else if Char.isSpace c
		       then SOME(Symbol "-", ns)
		       else scan_symbol reader ns

fun scan_s_exp reader stream = let
    val s = StringCvt.skipWS reader stream
    fun recur stream acc = case reader s of
			       NONE => NONE
			     | SOME (#")", ns) => SOME (rev acc, ns)
			     | SOME (_, _) => case (scan reader s) of
						  NONE => NONE
						| SOME (res, ns) => recur ns (res :: acc)
    fun recur2 s = case reader s of
    		       NONE => NONE
    		     | SOME (#")", ns) => SOME Nil
    		     | SOME (_, _) => case scan reader s of
    					  NONE => NONE
    					| SOME (res, ns) => case recur2 ns of
								NONE  => NONE 
							     | SOME v => SOME (Cell (res, v))
in recur stream []
end

fun scan reader stream = SOME ("blah", stream)

fun read stream = TextIO.scanStream scan_symbol stream
