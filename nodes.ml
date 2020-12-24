(*
type io =
  | GetInput
  | PrintOutput
*)

type node = 
  | NodeList of node list
  | ChangeVal of int
  | ChangePtr of int
  | Loop of node
  (*| IO of io*)