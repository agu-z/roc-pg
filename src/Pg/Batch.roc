module [Batch, succeed, with, sequence]

import Batch
import Cmd exposing [Cmd]

Batch a err : Batch.Batch a err

succeed : a -> Batch a err
succeed = Batch.succeed

with : Batch (a -> b) err, Cmd a err -> Batch b err
with = Batch.with

sequence : List (Cmd a err) -> Batch (List a) err
sequence = Batch.sequence
