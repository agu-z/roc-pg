interface Pg.Batch
    exposes [succeed, with, Batch]
    imports [
        Batch,
        Cmd.{ Cmd }
    ]

Batch a err : Batch.Batch a err

succeed : a -> Batch a err
succeed = Batch.succeed

with : Batch (a -> b) err, Cmd a err -> Batch b err
with = Batch.with