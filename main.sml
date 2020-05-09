val WIDTH = ref 80;
val FILES : string list ref = ref [];
val PRINTING : bool ref = ref false;

fun has_TODO (line : string) : string option =
    case explode line of
        (#"T" :: #"O" :: #"D" :: #"O" :: xs) => SOME line
      | (x :: xs)  => has_TODO (implode xs)
      | []         => NONE; 

fun has_FIXME (line : string) : string option =
    case explode line of
        (#"F" :: #"I" :: #"X" :: #"M" :: #"E" :: xs) => SOME line
      | (x :: xs)  => has_FIXME (implode xs)
      | []         => NONE;

local
    fun read_until_dot_lst (line : char list) : unit =
        case line of
            (#"." :: xs) => PRINTING := false
          | (x    :: xs) => (print (implode (x::[])); read_until_dot_lst xs)
          | [] => PRINTING := false
in
fun read_until_dot (line : string) : unit =
    read_until_dot_lst (explode line);
end;

fun cook_arg arg =
    case (explode arg) of
        ( #"-" :: #"w" :: #"=" :: a) => (case (Int.fromString (implode a)) of
                                             SOME i => WIDTH := i
                                           | _      => ())
      | (#"-" :: a ) => print "unrecognized arg" 
      | _  => FILES := arg :: (!FILES);

fun cook_args args =
    map cook_arg args;

local
    fun crdLn filename stream linum =
        case TextIO.inputLine stream of
            SOME line
            => (if size line > (!WIDTH)
                then (print (filename ^ ":" ^ Int.toString linum ^ ":" ^ line);
                      crdLn filename stream (linum + 1))
                else crdLn filename stream (linum + 1))
          | NONE => ()
                        
in
fun cdr (filename : string) : unit =
    let
        val i_stream = TextIO.openIn filename
    in
        crdLn filename i_stream 1
    end
end;

val main =
    cook_args (CommandLine.arguments ());
    map cdr (!FILES);
 
main;
