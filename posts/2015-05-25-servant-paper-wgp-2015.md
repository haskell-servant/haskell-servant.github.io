---
title: servant paper submitted to WGP 2015
author: Alp Mestanogullari
date: 2015-05-25 12:00
---

Sönke Hahn, Julian Arni, Andres Löh and myself have submitted a paper about *servant* to the [11th ACM SIGPLAN Workshop on Generic Programming](http://www.wgp-sigplan.org/farmer/doku.php?id=2015). Here's the abstract.

> We describe the design and motivation for Servant, an extensible, type-level
> DSL for describing Web APIs. Servant APIs are Haskell types. An API type can
> be interpreted in several different ways: as a server that processes
> requests, interprets them and dispatches them to appropriate handlers;
> as a client that can correctly query the endpoints of the API; as systematic
> documentation for the API; and more. Servant is fully extensible: the API
> language can be augmented with new constructs, and new interpretations can
> be defined. The key Haskell features making all this possible are data
> kinds, (open) type families and (open) type classes. The techniques we use
> are reminiscent of general-purpose generic programming. However, where most
> generic programming libraries are interested in automatically deriving
> programs for a large class of datatypes from many different domains, we are
> only interested in a small class of datatypes that is used in the DSL for
> describing APIs.

The full draft is available [here](http://alpmestan.com/servant/servant-wgp.pdf).
