This file is the upstream MIT license that governs the use of the
vendored copy of Prototype.js shipped inside javamelody-core at
`net/bull/javamelody/resource/prototype.js`.

The local fork has been modified relative to upstream (notably to remove
the `String#stripTags` and `String#unescapeHTML` functions in mitigation
of CVE-2020-27511, and to rewrite the `Version` literal at Maven build
time). See `javamelody-core/prototype.js.CHANGES.md` in the JavaMelody
source distribution for the full list of local modifications and the
upstream provenance (vendor commit, SHA-256, and upstream release tag).

The text below is the canonical MIT license referenced from the
`prototype.js` file header. Upstream's own LICENSE file
(https://github.com/prototypejs/prototype/blob/1.7.3/LICENSE) is
malformed in that it omits the "shall be included in all copies"
clause; the canonical form is reproduced here in full so consumers of
the JAR can rely on the conventional MIT terms.

----------------------------------------------------------------------

Copyright (c) 2005-2010 Sam Stephenson

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
