# Bachelor Thesis Task Description

The backend systems of large internet companies such as Facebook, Twitter and Amazon assemble via expensive network I/O whose optimization is strictly at odds with writing concise and modular code.
These companies use an approach called service-oriented architecture (SOA) to build their software infrastructure. A (small) program, a so-called service, exposes itself via the network instead of via a library. The backend system in these companies is constructed of many services that in turn call other services. The benefits are fault isolation when a services crashes and high flexibility to integrate new services or update old ones. However, these come at a price: network I/O which is often expensive. In turn, optimizing network I/O in the program code makes code inconcise and breaks modularity.

At this chair, we work on Ohua a programming language for systems that enables implicit parallel execution. Recent work showed that Ohua also provides a promising approach to address the I/O optimization challenge of SOA programs. Ohua programs compile to a dataflow graph where these optimizations can easily be introduced without the developer even noticing.

We already implemented the algorithms and performed experiments that verify our claims. However, so far our investigation lacked nested structures, conditionals and functions. These are vital aspects of a language for which we have to show that our approach, called Ÿauhau, achieves similar superior results.

This Bachelors Thesis shall complete this work by performing the following tasks:

1. Ohua does not support functions on the algorithm level properly and therefore needs to be extended towards that.
- Extend the Ÿauhau algorithms to support deeply nested smap-structures, conditionals and functions.
- Extend our random code graph benchmarking tool to produce programs that include these code patterns.
- Evaluate Ÿauhau’s performance for these code patterns and compare towards state-of-the-art (Haxl and Muse).

