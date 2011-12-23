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

import java.io.Writer

/**
 * Root of the `Node` ADT, representing the different types of supported XML
 * nodes which may appear in an XML fragment.  The ADT itself has the following
 * shape (Haskell syntax):
 *
 * {{{
 * type Prefix = Maybe String
 * type Scope = Map String String
 *
 * data Node = ProcInstr String String
 *           | Elem Prefix String Attributes Scope (Group Node)
 *           | Text String
 *           | CDATA String
 *           | EntityRef String
 * }}}
 *
 * For those that don't find Haskell to be the clearest explanation of what's
 * going on in this type, here is a more natural-language version.  The `Node`
 * trait is sealed and has exactly four subclasses, each implementing a different
 * type of XML node.  These four classes are as follows:
 *
 * <ul>
 * <li>[[com.codecommit.antixml.ProcInstr]] – A processing instruction consisting
 * of a target and some data</li>
 * <li>[[com.codecommit.antixml.Elem]] – An XML element consisting of an optional
 * prefix, a name (or identifier), a set of attributes, a set of namespace mappings 
 * in scope and a sequence of child nodes</li>
 * <li>[[com.codecommit.antixml.Text]] – A node containing a single string, representing
 * character data in the XML tree</li>
 * <li>[[com.codecommit.antixml.CDATA]] – A node containing a single string, representing
 * ''unescaped'' character data in the XML tree</li>
 * <li>[[com.codecommit.antixml.EntityRef]] – An entity reference (e.g. `&amp;`)</li>
 * </ul>
 */
 sealed trait Node {
  /** 
   * Returns the children of this node. If the node is an [[com.codecommit.antixml.Elem]], 
   * then this method returns the element's children.  Otherwise, it returns an empty
   * [[com.codecommit.antixml.Group]].
   */
  def children = Group.empty[Node]
 }

private[antixml] object Node {

  /* http://www.w3.org/TR/xml/#NT-Char */
  // TODO we are missing codepoints \u10000-\u10FFFF (i.e. those above 16 bits) here
  private[this] val CharRegex = "[\u0009\u000A\u000D\u0020-\uD7FF\uE000-\uFFFD]*"r

  def hasOnlyValidChars(value: String) = CharRegex.pattern.matcher(value).matches
  // TODO we should probably find a way to propagate custom entities from DTDs
  /* http://www.w3.org/TR/xml/#NT-CharData */
  def escapeText(text: String) = text flatMap {
    case '&' => "&amp;"
    case '<' => "&lt;"
    case '>' => "&gt;" // text may not contain ]]>, this is a way to avoid that
    case c => List(c)
  }

  /* http://www.w3.org/TR/xml/#NT-AttValue */
  def quoteAttribute(value: String) = {
    if (value.contains("\"")) {
      "'" + (value flatMap {
        case '&' => "&amp;"
        case '<' => "&lt;"
        case '\'' => "&apos;"
        case c => List(c)
      }) + "'"
    } else {
      "\"" + (value flatMap {
        case '&' => "&amp;"
        case '<' => "&lt;"
        case '"' => "&quot;"
        case c => List(c)
      }) + "\""
    }
  }
}

/**
 * A processing instruction consisting of a `target` and some `data`.  For example:
 *
 * {{{
 * <?xml version="1.0"?>
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * ProcInstr("xml", "version=\"1.0\"")
 * }}}
 */
case class ProcInstr(target: String, data: String) extends Node {
  override def toString = "<?" + target + " " + data + "?>"
}

/**
 * An XML element consisting of an optional namespace prefix, a name (or identifier), a
 * set of attributes, a namespace prefix scope (mapping of prefixes to namespace URIs),
 * and a sequence of child nodes.
 * For example:
 *
 * {{{
 * <span id="foo" class="bar">Lorem ipsum</span>
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * Elem(None, "span", Attributes("id" -> "foo", "class" -> "bar"), Map(), Group(Text("Lorem ipsum")))
 * }}}
 *
 * TODO: Clean up naming. Options:
 * 1) Rename prefix to namespace, scope to namespaces
 * 2) Remove prefix, rename scope to namespace(s), assume that the first is the name of the current element
 * 3) Rename prefix to namespace, scope to namespaces.
 */
case class Elem(prefix: NamespaceBinding, name: String, attrs: Attributes, scope: NamespaceBinding, override val children: Group[Node]) extends Node with Selectable[Elem] {
  import Elem.isValidName

  if (! isValidName(name)) {
    throw new IllegalArgumentException("Illegal element name, '" + name + "'")
  }
  
  /**
   * See the `canonicalize` method on [[com.codecommit.antixml.Group]].
   */
  def canonicalize = copy(children=children.canonicalize)
  
  override def toString = {
    val sw = new java.io.StringWriter() 
    val xs = XMLSerializer()
    xs.serialize(this, sw)
    sw.toString
  }
  
  def toGroup = Group(this)
}

object Elem extends ((NamespaceBinding, String, Attributes, NamespaceBinding, Group[Node]) => Elem) {
  private[this] val NameRegex = {
    val nameStartChar = """:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD"""
    "[" + nameStartChar + "][" + nameStartChar + """\-\.0-9\u00B7\u0300-\u036F\u203F-\u2040]*"""r
  }
  def isValidName(string: String) = NameRegex.pattern.matcher(string).matches
}

/*
 * The URI is required and cannot be an empty namespace name. See "2.2 Use of URIs as Namespace Names", "The empty
 * string, though it is a legal URI reference, cannot be used as a namespace name.".
 *
 * TODO: I wonder, should NB be exactly like a linked list that always end with an EmptyNB? That way there won't be
  * any need for any options probably giving significant performance savings when matching and extracting.
 */
sealed trait NamespaceBinding {
  def uri: Option[String]

  def parent: Option[NamespaceBinding]

  def isEmpty: Boolean

//  def append(child: NamespaceBinding): NamespaceBinding = child match {
//    case EmptyNamespaceBinding => this
//    case UnprefixedNamespaceBinding(uri, _) => new UnprefixedNamespaceBinding(uri, Some(this))
//    case PrefixedNamespaceBinding(prefix, uri, _) => PrefixedNamespaceBinding(prefix, uri, Some(this))
//  }

  def append(uri: String): NamespaceBinding = new UnprefixedNamespaceBinding(uri, Some(this))

  def append(prefix: String, uri: String): NamespaceBinding = new PrefixedNamespaceBinding(prefix, uri, Some(this))

  def findPrefix(prefix: String): Option[NamespaceBinding] = this match {
    case UnprefixedNamespaceBinding(_, _) if(prefix.isEmpty) => Some(this)
    case UnprefixedNamespaceBinding(_, Some(parent)) => parent.findPrefix(prefix)
    case UnprefixedNamespaceBinding(_, None) => None
    case PrefixedNamespaceBinding(p, _, Some(parent)) => if(p.equals(prefix)) Some(this) else parent.findPrefix(prefix)
    case PrefixedNamespaceBinding(p, _, None) => if(p.equals(prefix)) Some(this) else None
    case EmptyNamespaceBinding => None
  }

  /**
   * This is probably not a good idea.
   */
  def looseParent: NamespaceBinding

//  def -(parent: NamespaceBinding): NamespaceBinding = {
//    val b = new ListBuffer[NamespaceBinding]
//    var these = Some(this)
//    while (!these.isEmpty) {
//      if (these.head != x)
//        b += these.head
//      these = these.parent
//    }
//  }

  def toList: List[NamespaceBinding] = {
    def toStream(nb: NamespaceBinding): Stream[NamespaceBinding] = Stream.cons(nb, if(nb.parent.isDefined) toStream(nb.parent.get) else Stream.empty)
    toStream(this).toList
  }
}

object NamespaceBinding {
  val empty: NamespaceBinding = EmptyNamespaceBinding

  def apply(t: (String, String)) = new PrefixedNamespaceBinding(t._1, t._2, None)

  def apply(t: (String, String), parent: NamespaceBinding) = new PrefixedNamespaceBinding(t._1, t._2, Some(parent))

  def apply(prefix: String,  uri: String, parent: NamespaceBinding) = new PrefixedNamespaceBinding(prefix, uri, Some(parent))

  def apply(prefix: String,  uri: String) = new PrefixedNamespaceBinding(prefix, uri, None)

  def apply(uri: String, parent: NamespaceBinding) = new UnprefixedNamespaceBinding(uri, Some(parent))

  def apply(uri: String) = new UnprefixedNamespaceBinding(uri, None)
}

case object EmptyNamespaceBinding extends NamespaceBinding {
  def uri = None
  def parent = None
  def isEmpty = true

  override def append(uri: String): NamespaceBinding = new UnprefixedNamespaceBinding(uri, None)

  override def append(prefix: String, uri: String): NamespaceBinding = new PrefixedNamespaceBinding(prefix, uri, None)

  def looseParent = this

  override def toList = List.empty
}

case class PrefixedNamespaceBinding(prefix: String, _uri: String,  override val parent: Option[NamespaceBinding] = None) extends NamespaceBinding {
  if (! Elem.isValidName(prefix)) {
    throw new IllegalArgumentException("Illegal namespace prefix, '" + prefix + "'")
  }

  def uri = Some(_uri)
  def isEmpty = false
  def looseParent = PrefixedNamespaceBinding(prefix, _uri, None)
}

case class UnprefixedNamespaceBinding(_uri: String,  override val parent: Option[NamespaceBinding] = None) extends NamespaceBinding {
  def uri = None
  def isEmpty = false
  def looseParent = UnprefixedNamespaceBinding(_uri, None)
}

/**
 * A node containing a single string, representing character data in the XML tree.
 * For example:
 *
 * {{{
 * Lorem ipsum &amp; dolor sit amet
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * Text("Lorem ipsum & dolor sit amet")
 * }}}
 *
 * Note that reserved characters (as defined by the XML 1.0 spec) are escaped
 * when calling `toString`.  Thus, if you invoke `toString` on the `Text` node
 * given in the example above, the result will reverse back into the original
 * text, including the `&amp;` escape.  If you need a text representation which
 * does ''not'' escape characters on output, use [[com.codecommit.antixml.CDATA]].
 */
case class Text(text: String) extends Node {
  import Node.hasOnlyValidChars

  if (!hasOnlyValidChars(text))
    throw new IllegalArgumentException("Illegal character in text '" + text + "'")

  override def toString = Node.escapeText(text)
}

/**
 * A node containing a single string, representing unescaped character data in
 * the XML tree.  For example:
 *
 * {{{
 * <![CDATA[Lorem ipsum & dolor sit amet]]>
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * CDATA("Lorem ipsum & dolor sit amet")
 * }}}
 *
 * Note that reserved characters (as defined by the XML 1.0 spec) are ''not''
 * escaped when calling `toString`.  If you need a text representation which
 * performs escaping, use [[com.codecommit.antixml.Text]]
 */
case class CDATA(text: String) extends Node {
  import Node.hasOnlyValidChars

  if (text.contains("]]>"))
    throw new IllegalArgumentException("CDATA nodes can't contain ']]>'")

  if (!hasOnlyValidChars(text))
    throw new IllegalArgumentException("Illegal character in CDATA '" + text + "'")

  override def toString = "<![CDATA[" + text + "]]>"
}

/**
 * A node representing an entity reference. For example:
 *
 * {{{
 * &hellip;
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * EntityRef("hellip")
 * }}}
 */
case class EntityRef(entity: String) extends Node {
  import Node.hasOnlyValidChars

  if (!hasOnlyValidChars(entity))
    throw new IllegalArgumentException("Illegal character in EntityRef '" + entity + "'")

  override def toString = "&" + entity + ";"
}
