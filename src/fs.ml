(* Filesystem helper functions *)
open Pcre;;

(* List all files in a directory *)
let list_all = function
  | path when Sys.is_directory path ->
    Ok (path
        |> Sys.readdir
        |> Array.to_list
        |> List.map (Filename.concat path))
  | path ->
    Error (path ^ " is not a directory")

(* Search for a file in path by regex rx *)
let fs_glob path rx =
  match list_all path with
  | Ok files -> Ok (List.filter (Pcre.pmatch ~rex: rx) files)
  | Error e -> Error e (* Quirk of the ocaml type system. *)
