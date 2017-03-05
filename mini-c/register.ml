
type t = string

let r = ref 0
let fresh () = incr r; "#" ^ string_of_int !r
let is_pseudo r = r.[0] = '#'

module S = Set.Make(String)

type set = S.t

let set_of_list l = List.fold_right S.add l S.empty

let print = Format.pp_print_string

let print_8bit fmt r =
  let newr = match r with
  | "%rax" -> "%al"
  | "%rbx" -> "%bl"
  | "%rcx" -> "%cl"
  | "%rdx" -> "%dl"
  | "%rsi" -> "%sil"
  | "%rdi" -> "%dil"
  | "%rbp" -> "%bpl"
  | "%rsp" -> "%spl"
  | "%r8" -> "%r8b"
  | "%r9" -> "%r9b"
  | "%r10" -> "%r10b"
  | "%r11" -> "%r11b"
  | "%r12" -> "%r12b"
  | "%r13" -> "%r13b"
  | "%r14" -> "%r14b"
  | "%r15" -> "%r15b"
  | _ -> failwith "register does not exist" in
  Format.pp_print_string fmt newr

let print_set fmt s =
  Pp.print_list Pp.comma Format.pp_print_string fmt (S.elements s)

module M = Map.Make(String)

type 'a map = 'a M.t

(* registres physiques x86-64 *)

let rax = "%rax"
let rdi = "%rdi"
let rdx = "%rdx"

let parameters =
  [ rdi; "%rsi"; rdx; "%rcx"; "%r8"; "%r9" ]

let result =
  rax

let caller_saved =
  rax :: "%r10" :: parameters

let callee_saved =
  [ "%rbx"; "%r12"; (* "%r13"; "%r14"; "%r15" *) ]

let allocatable = set_of_list (caller_saved @ callee_saved)

let rsp =
  "%rsp"

let tmp1, tmp2 =
  "%rbp", "%r11"

let al = "%al"

let is_hw r = r.[0] = '%'
