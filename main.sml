val WIDTH = ref 80;
val FILES : string list ref = ref [];

fun cook_arg arg
    = case (explode arg) of
          ( #"-" :: #"w" :: #"=" :: a)
         => (case (Int.fromString (implode a)) of
                    SOME i => WIDTH := i
                  | _      => ())
       | (#"-" :: a )
         => print "unrecognized arg" 
       | _  => FILES := arg :: (!FILES);

fun cook_args args
    = map cook_arg args;
	 

val rec printList
    = fn xs =>
         case xs of
             [] => ()
           | (x :: xs) => (print (x ^ "\n") ; printList xs);


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

val main
    = cook_args (CommandLine.arguments ());
      map cdr (!FILES);
 
main;
