open! Core
open Angstrom
open Formula

type formula = Formula.t

(* Whitespace handling *)
let whitespace =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let token p = p <* whitespace
let parens p = token (char '(') *> p <* token (char ')')
let symbol s = token (string s)

(* Identifiers *)
let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

let ident : string Angstrom.t =
  lift2 (fun c s -> String.make 1 c ^ s) (satisfy is_alpha) (take_while is_alphanum)
  >>= fun s -> whitespace *> return s
;;

(* Variables *)
(* var is not used anywhere in code, but the parser needs it, so we suppress the warning. *)
let var : var Angstrom.t = ident >>= fun name -> return (mk_var name) [@@warning "-32"]

let typed_var : var Angstrom.t =
  ident
  >>= fun name ->
  option None (char ':' *> ident >>| fun t -> Some t)
  >>= fun typ ->
  match typ with
  | None -> return (mk_var name)
  | Some t -> return (mk_var ~typ:t name)
;;

(* Parameters tuple *)
let tuple : var list Angstrom.t = parens (sep_by (token (char ',')) typed_var)

(* Function application *)
let fun_term : Formula.t Angstrom.t =
  ident
  >>= fun name ->
  option None (char '#' *> ident >>| fun i -> Some i)
  >>= fun index ->
  tuple
  >>= fun params ->
  match index with
  | None -> return (mk_fun name params)
  | Some idx -> return (mk_fun_indexed name idx params)
;;

(* Forward declaration for recursive parser *)
let term : Formula.t Angstrom.t ref = ref (fail "term not initialized")

(* Boolean constants *)
let true_lit = symbol "True" *> return True
let false_lit = symbol "False" *> return False

(* Equality *)
let equal : t Angstrom.t =
  typed_var
  >>= fun v1 ->
  token (char '=') *> typed_var >>= fun v2 -> return (Equal (Var v1, Var v2))
;;

let not_equal : t Angstrom.t =
  typed_var
  >>= fun v1 ->
  (token (string "≠") <|> token (string "!=")) *> typed_var
  >>= fun v2 -> return (Neg (Equal (Var v1, Var v2)))
;;

let eq_term = equal <|> not_equal

(* Simple terms (non-recursive) *)
let simple_term : t Angstrom.t =
  choice [ true_lit; false_lit; fun_term; eq_term; (typed_var >>| fun v -> Var v) ]
;;

(* Helper for left-associative binary operators *)
let chainl1 p op =
  let rec go acc = lift2 (fun f x -> f acc x) op p >>= go <|> return acc in
  p >>= go
;;

(* Complete term parser with proper precedence *)
let () =
  term
  := fix (fun self ->
       whitespace
       *>
       (* Quantifiers - defined inside to use self recursively *)
       let exists =
         token (string "∃") *> sep_by1 (token (char ',')) typed_var
         >>= fun vars ->
         option () (token (char '.') >>| fun _ -> ()) *> self
         >>= fun t -> return (mk_exists vars t)
       in
       let forall =
         token (string "∀") *> sep_by1 (token (char ',')) typed_var
         >>= fun vars ->
         option () (token (char '.') >>| fun _ -> ()) *> self
         >>= fun t -> return (mk_forall vars t)
       in
       let quantifier = exists <|> forall in
       (* Helper to parse single-letter operator followed by non-alphanumeric *)
       let op_x =
         string "X" *> peek_char
         >>= function
         | Some c when is_alphanum c -> fail "X followed by alphanumeric"
         | _ -> whitespace *> return ()
       in
       let op_g =
         string "G" *> peek_char
         >>= function
         | Some c when is_alphanum c -> fail "G followed by alphanumeric"
         | _ -> whitespace *> return ()
       in
       let op_f =
         string "F" *> peek_char
         >>= function
         | Some c when is_alphanum c -> fail "F followed by alphanumeric"
         | _ -> whitespace *> return ()
       in
       let atom =
         fix (fun atom_parser ->
           choice
             [ parens self
             ; (* Temporal operators must not be followed by alphanumeric chars *)
               ((op_x <|> (token (string "◯") >>| fun _ -> ())) *> atom_parser
                >>| fun t -> Next t)
             ; ((op_g <|> (token (string "☐") >>| fun _ -> ())) *> atom_parser
                >>| fun t -> Globally t)
             ; ((op_f <|> (token (string "♢") >>| fun _ -> ())) *> atom_parser
                >>| fun t -> Finally t)
             ; ((token (string "¬") <|> token (string "!") >>| fun _ -> ()) *> atom_parser
                >>| fun t -> Neg t)
             ; quantifier
             ; simple_term
             ])
       in
       let until_level =
         atom
         >>= fun left ->
         option None (token (string "U") >>| fun _ -> Some ())
         >>= function
         | None -> return left
         | Some _ -> atom >>= fun right -> return (Until (left, right))
       in
       let wuntil_level =
         until_level
         >>= fun left ->
         option None (token (string "W") >>| fun _ -> Some ())
         >>= function
         | None -> return left
         | Some _ -> until_level >>= fun right -> return (WUntil (left, right))
       in
       let and_level =
         chainl1
           wuntil_level
           (token (string "∧") <|> token (string "&&") >>| fun _ -> fun a b -> And (a, b))
       in
       let or_level =
         chainl1
           and_level
           (token (string "∨") <|> token (string "||") >>| fun _ -> fun a b -> Or (a, b))
       in
       let implies_level =
         or_level
         >>= fun left ->
         option
           None
           (token (string "⟹")
            <|> token (string "=>")
            <|> token (string "->")
            >>| fun _ -> Some ())
         >>= function
         | None -> return left
         | Some _ -> self >>= fun right -> return (Implies (left, right))
       in
       let equiv_level =
         implies_level
         >>= fun left ->
         option None (token (string "<->") >>| fun _ -> Some ())
         >>= function
         | None -> return left
         | Some _ -> self >>= fun right -> return (Equiv (left, right))
       in
       equiv_level)
;;

(* Main parsing function *)
let parse_formula (s : string) : (formula, string) result =
  match parse_string ~consume:All !term s with
  | Ok formula -> Ok formula
  | Error msg -> Error msg
;;

(* Convenience function that raises exception on error *)
let parse_formula_exn (s : string) : formula =
  match parse_formula s with
  | Ok f -> f
  | Error msg -> failwith ("Parse error: " ^ msg)
;;
