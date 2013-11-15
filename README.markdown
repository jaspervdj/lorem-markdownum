# lorem-markdownum

<http://jaspervdj.be/lorem-markdownum/>

## Introduction

*Lorem Markdownum* is a [lorem ipsum] generator for [markdown].

[lorem ipsum]: http://www.lipsum.com/
[markdown]: http://daringfireball.net/projects/markdown/

## Code

This repository currently only contains the Haskell *library* (responsible for
actually generating the markdown). Due to the fact that the *server* (which
wraps this library) uses some proprietary components, I cannot open-source it
yet.

## HTTP API

An HTTP API is available. You can use it by sending a `GET` request to
`/markdown.txt`.

    $ curl 'jaspervdj.be/lorem-markdownum/markdown.txt'
    # In diva in Alcides cumque postquam reserato

    ## Proxima bellare te tractata Atrides exercet

    Lorem markdownum, aequent vocem dixit tamen quidem crimine in maris protinus
    moror *telluris* magno, marinae Latonae. Opaca tamquam ligari!

    ...

Arguments can be passed as query parameters, e.g.:

    $ curl 'jaspervdj.be/lorem-markdownum/markdown.txt?underline-headers=on'
    In diva in Alcides cumque postquam reserato
    ===========================================

    Proxima bellare te tractata Atrides exercet
    -------------------------------------------

    Lorem markdownum, aequent vocem dixit tamen quidem crimine in maris protinus
    moror *telluris* magno, marinae Latonae. Opaca tamquam ligari!

    ...

These are the supported parameters:

- `no-headers`
- `no-code`
- `no-quotes`
- `no-lists`
- `no-inline-markup`
- `reference-links`
- `underline-headers`
- `underscore-em`
- `underscore-strong`
