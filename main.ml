let count_bytes = ref false
let count_lines = ref false
let count_words = ref false
let filenames = ref []

let tee f x = f x; x

let is_space = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false

let spec =
  [("-c",
      Arg.Set count_bytes,
      "The number of btes in each input file is written to the standard output.");
   ("-l",
      Arg.Set count_lines,
      "The number of lines in each input file is written to the standard output.");
   ("-w",
      Arg.Set count_words,
      "The number of words in each input file is written to the standard output.")]

let print_filesize (filename, lines, bytes, words) =
  if !count_lines then
    Printf.printf "%7d " lines;
  if !count_words then
    Printf.printf "%7d " words;
  if !count_bytes then
    Printf.printf "%7d " bytes;
  print_endline filename

let () =
  Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: wc [-clw] [file ...]";
  if not !count_bytes && not !count_words && not !count_lines then begin
    count_bytes := true;
    count_words := true;
    count_lines := true
  end;
  List.rev !filenames
  |> List.map (fun filename ->
      let channel = open_in filename in
      let bytes = ref 0 in
      let lines = ref 0 in
      let words = ref 0 in
      let prev = ref ' ' in
      begin try 
        while true do
          let c = input_char channel in
          incr bytes;
          if c = '\n' then
            incr lines;
          if is_space c && not (is_space !prev) then
            incr words;
          prev := c
        done
      with End_of_file -> ()
      end;
      close_in channel;
      (filename, !lines, !bytes, !words))
  |> tee (List.iter print_filesize)
  |> List.fold_left
      (fun
        (total_lines, total_bytes, total_words)
        (_, lines, bytes, words) ->
          (total_lines + lines, total_bytes + bytes, total_words + words))
      (0, 0, 0)
  |> (fun (lines, bytes, words) ->
      if List.length !filenames > 1 then
        print_filesize ("total", lines, words, bytes))
