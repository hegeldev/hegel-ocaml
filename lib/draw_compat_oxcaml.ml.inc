(* The compiler fills these arguments at the caller, including through aliases
   such as Hegel.draw. Forward explicitly so the core sees the original site. *)
let draw ?label ~(loc : [%call_pos]) tc gen =
  Generators_core.draw ?label ~loc tc gen
;;

let draw_named ~label ~(loc : [%call_pos]) ~repeatable tc gen =
  Generators_core.draw_named ~label ~loc ~repeatable tc gen
;;
