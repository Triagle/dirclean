(* Filesystem helper functions *)
open Unix;;
open Pcre;;
open Inotify;;
exception InvalidPath of string
(* List all files in a directory *)
let list_all = function
  | path when Sys.is_directory path ->
    Ok (path
        |> Sys.readdir
        |> Array.to_list
        |> List.map (Filename.concat path))
  | path ->
    Error path

(* Search for a file in path by regex rx *)
let fs_glob path f =
  match list_all path with
  | Ok files -> Ok (List.filter f files)
  | Error e -> Error e (* Quirk of the ocaml type system. *)

(* Get age of file *)
let age_of file =
  (Unix.stat file).st_mtime

(* Move src_path into dest_folder (a folder) *)
let move dest_folder src_path =
  let dest = Filename.concat dest_folder (Filename.basename src_path) in
  Lwt_unix.rename src_path dest

(* Recursively create path *)
let create_path path =
  let split = Pcre.split ~pat:"/" path in
  let rec _aux cur_path path_left =
    match (cur_path, path_left) with
    | (_, []) -> ()
    | (path, x::xs) when Sys.file_exists path == false -> Unix.mkdir path 0o700; _aux (Filename.concat path x) xs
    | (path, x::xs) when Sys.is_directory path -> _aux (Filename.concat path x) xs
    | (_, _) -> raise (InvalidPath path) in
  _aux "/" split;
  Unix.mkdir path 0o700;
  ()

let get_user_home user =
  let user = getpwnam user in
  user.pw_dir

let expand_path = Pcre.replace_first ~rex: (regexp "~/") ~templ: ((get_user_home (getenv "USER")) ^ "/")
