Closure External Producer
=========================

    Typing all the way down.

When creating a web service using Json message, it is useful to
use the "Google Closure Compiler" to validate the client side
javascript. Type verification in the closure compiler require
type definitions known as externs.


When coding in haskell, all the types are _already_ there, so
let's try to use the compiler to provide the type definitions
for us, or at least, help us in this task.

Serialization
-------------
The Aeson library provide an useful typeclass to serialize to
JSON data. The ideal solution is to provide one description
for the typing and the serialization.

The trick is that the typing language is richer than the
serializable one, for instance JSON cannot serialize function
nor tuples, thus need to be excluded of serialization capability.

