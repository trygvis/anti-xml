/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of "Anti-XML" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.codecommit.antixml

import org.specs2.mutable._

class StAXSpecs extends Specification {
  object StAXParser extends StAXParser

  "StAXParser" should {
    "parse a simple element" in {
      StAXParser.fromString("<a/>") mustEqual Elem(NamespaceBinding.empty, "a", Attributes(), NamespaceBinding.empty, Group())
    }
    
    "parse an element with namespace" in {
      StAXParser.fromString("<a xmlns='urn:a'/>") mustEqual Elem(NamespaceBinding("urn:a"), "a", Attributes(), NamespaceBinding("urn:a"), Group())
    }

    "parse an element with default namespace prefix" in {
      StAXParser.fromString("<a xmlns='urn:a'><b/></a>") mustEqual Elem(NamespaceBinding("urn:a"), "a", Attributes(), NamespaceBinding("urn:a"), Group(Elem(NamespaceBinding("urn:a"), "b", Attributes(), NamespaceBinding("urn:a"), Group())))
    }

    "parse an element with default namespace prefix" in {
      StAXParser.fromString("<a xmlns='urn:a' xmlns:x='urn:x'><b/></a>") mustEqual Elem(NamespaceBinding("urn:a"), "a", Attributes(), NamespaceBinding("x", "urn:x", NamespaceBinding("urn:a")), Group(Elem(NamespaceBinding("urn:a"), "b", Attributes(), NamespaceBinding("x", "urn:x", NamespaceBinding("urn:a")), Group())))
    }

    "parse an element with default namespace prefix and child element with a re-defined default namespace" in {
      StAXParser.fromString("<a xmlns='urn:a'><b xmlns='urn:b'/></a>") mustEqual Elem(NamespaceBinding("urn:a"), "a", Attributes(), NamespaceBinding("urn:a"), Group(Elem(NamespaceBinding("urn:b"), "b", Attributes(), NamespaceBinding("urn:b", NamespaceBinding("urn:a")), Group())))
    }

    "parse a simpleString and generate an Elem" in {
      StAXParser.fromString("<a:a xmlns:a='urn:a'>hi<b attr='value' /> there</a:a>") mustEqual Elem(PrefixedNamespaceBinding("a", "urn:a"), "a", Attributes(), NamespaceBinding("a", "urn:a"), Group(Text("hi"), Elem(NamespaceBinding.empty, "b", Attributes(QName(None, "attr") -> "value"), NamespaceBinding("a", "urn:a"), Group()), Text(" there")))
    }

    "parse a simpleString with an non-prefixed namespace" in {
      StAXParser.fromString("<a xmlns='urn:a'/>") mustEqual Elem(NamespaceBinding("urn:a"), "a", Attributes(), NamespaceBinding("urn:a"), Group())
    }

    "parse a simpleString with both a namespace and an attribute" in {
      StAXParser.fromString("<a xmlns='urn:a' key='val' />") mustEqual Elem(NamespaceBinding("urn:a"), "a", Attributes("key"->"val"), NamespaceBinding("urn:a"), Group())
    }

    "parse a simpleString with both a namespace and an attribute" in {
      StAXParser.fromString("<a xmlns='urn:a' xmlns:b='urn:b-ns' key='val'><b:foo/></a>") mustEqual Elem(NamespaceBinding("urn:a"), "a", Attributes("key"->"val"), NamespaceBinding("b", "urn:b-ns", NamespaceBinding("urn:a")), Group(Elem(NamespaceBinding("b", "urn:b-ns"), "foo", Attributes(), NamespaceBinding("b", "urn:b-ns", NamespaceBinding("urn:a")), Group())))
    }

    "reuse namespace binding objects, 1" in {
      val e = StAXParser.fromString("<a xmlns='urn:a'><b/></a>")
      e.prefix must beTheSameAs(e.children.head.asInstanceOf[Elem].prefix)
    }

    "reuse namespace binding objects even if it's re-declared" in {
      val e = StAXParser.fromString("<a xmlns='urn:a'><b xmlns='urn:a'/></a>")
      e.prefix must beTheSameAs(e.children.head.asInstanceOf[Elem].prefix)
    }
  }
}
