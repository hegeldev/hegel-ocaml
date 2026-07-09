include Generators_core
include Generators_primitives
include Generators_collections
include Generators_combinators
include Generators_functions

module Ppx_internal = struct
  module Labels = Generators_core.Labels

  type collection = Generators_core.collection =
    { mutable finished : bool
    ; mutable collection_id : int option
    ; min_size : int
    ; max_size : int option
    }

  let max_filter_attempts = Generators_core.max_filter_attempts
  let group = Generators_core.group
  let discardable_group = Generators_core.discardable_group
  let resolve_draw = Generators_core.resolve_draw
  let new_collection = Generators_core.new_collection
  let collection_more = Generators_core.collection_more
  let collection_reject = Generators_core.collection_reject
  let pool_values = Generators_core.pool_values
  let composite_with_label = Generators_core.composite_with_label
end
