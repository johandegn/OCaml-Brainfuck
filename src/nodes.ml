type node =
  | Loop of node list
  | ChangeVal of int
  | ChangePtr of int
  | PrintValue
  | InputValue