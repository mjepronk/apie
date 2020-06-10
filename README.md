# Apie

> NOTE: This project is not for general consumption yet.

This program provides a very minimal backend API for an event sourcing system.

The main features are:

- an append-only event stream that can be used to record all data mutations
- content-based storage (for binary data)
- authenticate users using HTTP basic authentication

## Event stream

The event stream is an append-only log of all changes made to structured data of
your application. The client can use this event stream to create a projection on
the structured data, basically the state of your application data at a given
moment.

The advantages of storing your application data as an event stream are:

- view all changes to the application data (for auditing, rollback of
  incorrect changes, "time travel" debugging, ...)
- synchronize distributed data (support offline applications, ...)

## CGI

The backend can be compiled to a statically compiled Linux binary that should
run as a CGI program on most hosting providers (on Linux). This requires the Nix
package manager to be installed (in order to build a binary that uses musl
instead of glibc).

Please make sure that the webserver passes the `Authorization` header
(`HTTP_AUTHORIZATION` environment variable) to the CGI script. For Apache you
can use the provided `cgi-bin/.htaccess`.

## Webserver

The backend can also run standalone using the Warp HTTP server. Please note
however that it's not an efficient implementation: we use file locks as
primitives for synchronization.

The plan is to create an efficient multi-threading server backend using STM.

## Examples

### Event Store

```bash
curl -X PUT -H "Content-Type: application/json" --data @event.json http://localhost:8000/events

curl http://localhost:8000/events

curl http://localhost:8000/events?from=0d0ef6946e84a7bb64c600709c05edda81d2ebec8b85ced529ae56a9de5eb9cb

curl http://localhost:8000/events?to=0d0ef6946e84a7bb64c600709c05edda81d2ebec8b85ced529ae56a9de5eb9cb

curl http://localhost:8000/events/0d0ef6946e84a7bb64c600709c05edda81d2ebec8b85ced529ae56a9de5eb9cb
```

### Content Store

```bash
curl -X PUT -H "Content-Type: application/pdf" -T ~/Documents/test.pdf http://localhost:8000/storage/

curl -X GET http://localhost:8000/storage/0d0ef6946e84a7bb64c600709c05edda81d2ebec8b85ced529ae56a9de5eb9cb --output test.pdf

curl -X DELETE http://localhost:8000/storage/0d0ef6946e84a7bb64c600709c05edda81d2ebec8b85ced529ae56a9de5eb9cb

```
