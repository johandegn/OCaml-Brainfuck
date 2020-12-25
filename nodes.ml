type node = 
  | Tuple of node * node
  | Loop of node
  | ChangeVal of int
  | ChangePtr of int
  | PrintValue
  | InputValue
  | Nop