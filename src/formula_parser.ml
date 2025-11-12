open! Core
open Angstrom
open Formula

(* Skip whitespace (spaces, tabs and newlines). *)
let whitespace =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

(* Token combinators that consume trailing whitespace. *)
let token p = p <* whitespace
let parens p = token (char '(') *> p <* token (char ')')
let symbol s = token (string s)

(* Identifier helpers. *)
let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

(* Parse an identifier (alpha followed by alphanumerics). *)
let ident : string Angstrom.t =
  lift2 (fun c s -> String.make 1 c ^ s) (satisfy is_alpha) (take_while is_alphanum)
  >>= fun s -> whitespace *> return s
;;

(* Variable parsers. The `var` parser is only used locally by the parsing
  combinators; the warning about unused values is suppressed. *)
let var : var Angstrom.t = ident >>= fun name -> return (mk_var name) [@@warning "-32"]

(* Parse an optionally-typed variable: `x` or `x:T`. *)
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
(* Parse a parenthesized tuple of typed variables: `(x, y:T, ...)`. *)
let tuple : var list Angstrom.t = parens (sep_by (token (char ',')) typed_var)

(* Function application *)

(* Parse a function application: `f(x,y)` or `f#i(x,y)` for indexed
    functions. *)
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
(* Forward declaration for recursive parser. *)
let term : Formula.t Angstrom.t ref = ref (fail "term not initialized")

(* Boolean constants parsers. *)
let true_lit = symbol "True" *> return True
let false_lit = symbol "False" *> return False

(* Equality *)

(* Parse equality between variables, e.g. `x = y`. *)
let equal : t Angstrom.t =
  typed_var
  >>= fun v1 ->
  token (char '=') *> typed_var >>= fun v2 -> return (Equal (Var v1, Var v2))
;;

(* Parse inequality using either `≠` or `!=`. *)
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
(* Helper for left-associative binary operators. *)
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
         (* Accept unicode ∃ or ascii \exists but ensure the keyword is not followed by alphanum
        (to avoid matching identifiers like "\\existsx"). Require a '.' after the var list. *)
         (string "∃" *> peek_char
          >>= (function
           | Some c when is_alphanum c -> fail "∃ followed by alphanumeric"
           | _ -> whitespace *> return ())
          <|> (string "\\exists" *> peek_char
               >>= function
               | Some c when is_alphanum c -> fail "\\exists followed by alphanumeric"
               | _ -> whitespace *> return ()))
         *> sep_by1 (token (char ',')) typed_var
         >>= fun vars ->
         (* Require a dot after the quantifier variable list. *)
         token (char '.') *> self >>= fun t -> return (mk_exists vars t)
       in
       let forall =
         (string "∀" *> peek_char
          >>= (function
           | Some c when is_alphanum c -> fail "∀ followed by alphanumeric"
           | _ -> whitespace *> return ())
          <|> (string "\\forall" *> peek_char
               >>= function
               | Some c when is_alphanum c -> fail "\\forall followed by alphanumeric"
               | _ -> whitespace *> return ()))
         *> sep_by1 (token (char ',')) typed_var
         >>= fun vars -> token (char '.') *> self >>= fun t -> return (mk_forall vars t)
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

(* Parse a formula from a string, returning (Ok formula) or (Error e). *)
let parse_formula s =
  parse_string ~consume:All !term s |> Result.map_error ~f:Error.of_string
;;

(* Like [parse_formula] but raises on parse errors. *)
let parse_formula_exn s = parse_formula s |> Or_error.ok_exn
