module [Batch, succeed, with, sequence]

import Batch
import Cmd exposing [Cmd]

Batch a err : Batch.Batch a err

# succeed : a -> Batch a err
succeed : _ -> Batch _ _
succeed = Batch.succeed

# with : Batch (a -> b) err, Cmd a err -> Batch b err
with : Batch (_ -> _) _, Cmd _ _ -> Batch _ _
with = Batch.with

# sequence : List (Cmd a err) -> Batch (List a) err
sequence : List (Cmd _ _) -> Batch (List _) _
sequence = Batch.sequence
