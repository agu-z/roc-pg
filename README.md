# roc-pg

Interface with PostgreSQL databases from pure Roc.

## Features

- [x] Connection handling
- [x] Parameterized queries
- [x] Decoding results
- [x] Decoding errors
- [ ] [`Decode`](https://www.roc-lang.org/builtins/Decode) ability implementation
- [ ] Authentication methods
  - [x] Cleartext password
  - [ ] MD5 password \*
  - [ ] SASL / SCRAM-SHA-256 \*
- [x] Prepared statements
- [x] Pipelining
  - [x] Applicative batches
  - [x] Sequence list of commands expecting same type
  - [x] 🚀 Parse and Describe once per unique SQL string
- [ ] Bulk copying
- [ ] Cursors
- [ ] SSL \*
- [ ] Connection pooling \*
- [ ] Notifications (listen/notify)
- [ ] Notices

\* Requires new platform primitives
