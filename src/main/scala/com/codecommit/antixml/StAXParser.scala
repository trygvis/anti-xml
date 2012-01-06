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

import util.VectorCase
import java.io.{InputStream, StringReader, Reader}
import javax.xml.stream._
import javax.xml.stream.XMLStreamConstants
import javax.xml.transform.stream.StreamSource

/*
<a/>
  START_ELEMENT, xmlReader.getNamespaceCount=0

<a xmlns='urn:a'/>
  START_ELEMENT, xmlReader.getNamespaceCount=1
  xmlReader.getNamespaceURI(i)=urn:a
  rawPrefix=null
  prefix=

 */

/**
 * An XML parser build on top of `javax.xml.stream`.  This implements the same
 * API as [[com.codecommit.antixml.SAXParser]], but the runtime performance is
 * on the order of 12% faster.
 */
class StAXParser extends XMLParser {

  override def fromInputStream(inputStream: InputStream): Elem =
    fromStreamSource(new StreamSource(inputStream))
  
  override def fromReader(reader: Reader): Elem =
    fromStreamSource(new StreamSource(reader))
  
  override def fromString(xml: String): Elem =
    fromReader(new StringReader(xml))
  
  private case class ElemBuilder(name: String, namespaces: NamespaceBinding, attrs: Attributes)

  private def fromStreamSource(source: StreamSource): Elem = {
    import XMLStreamConstants.{CHARACTERS, END_ELEMENT, START_ELEMENT}

    val xmlReader = XMLInputFactory.newInstance().createXMLStreamReader(source)
    var elems: List[ElemBuilder] = Nil
    // Map of all used namespaces as empty lists. Enables reuse of instances.
    var unprefixedNamespaceMap = Map[String, UnprefixedNamespaceBinding]()
    var prefixedNamespaceMap = Map[(String, String), PrefixedNamespaceBinding]()
    var scopes = NamespaceBinding.empty
    var results = VectorCase.newBuilder[Node] :: Nil
    val text = new StringBuilder
    while(xmlReader.hasNext) {
      xmlReader.next match {
        case `CHARACTERS` =>
          text.appendAll(xmlReader.getTextCharacters, xmlReader.getTextStart, xmlReader.getTextLength)
        case `END_ELEMENT` => {
          val elem = elems.head
          val parents = elems.tail
          val children = results.head
          val ancestors = results.tail

          val uri = xmlReader.getNamespaceURI
          val prefix = xmlReader.getPrefix

          val p = if(uri == null)
            NamespaceBinding.empty
          else {
            if(prefix == null || prefix.equals("")) {
              unprefixedNamespaceMap.get(uri) match {
                case Some(nb) => nb
                case None =>
                  val x = NamespaceBinding(uri)
                  unprefixedNamespaceMap = unprefixedNamespaceMap + (uri -> x)
                  x
              }
            }
            else {
              prefixedNamespaceMap.get((prefix, uri)) match {
                case Some(nb) => nb
                case None =>
                  val x = NamespaceBinding(prefix, uri)
                  prefixedNamespaceMap = prefixedNamespaceMap + ((prefix, uri) -> x)
                  x
              }
            }
          }

          if (text.size > 0) {
            children += Text(text.result())
            text.clear()
          }

          ancestors.head += Elem(p, elem.name, elem.attrs, elem.namespaces, Group fromSeq children.result)
          elems = parents
          results = ancestors
        }
        case `START_ELEMENT` => {
          if (text.size > 0) {
            results.head += Text(text.result())
            text.clear()
          }
          var i = 0
//          println("START_ELEMENT")
//          println("xmlReader.getNamespaceURI=" + xmlReader.getNamespaceURI)
//          println("xmlReader.getPrefix=" + xmlReader.getPrefix)
//          println("xmlReader.getLocalName=" + xmlReader.getLocalName)
//          println("xmlReader.getNamespaceCount=" + xmlReader.getNamespaceCount)
          while (i < xmlReader.getNamespaceCount) {
            val ns = xmlReader.getNamespaceURI(i)
//            println("xmlReader.getNamespaceURI(i)=" + xmlReader.getNamespaceURI(i))
            val rawPrefix = xmlReader.getNamespacePrefix(i)
//            println("rawPrefix=" + xmlReader.getNamespacePrefix(i))
            val prefix = if (rawPrefix != null) rawPrefix else "" 
//            println("prefix=" + prefix)
            scopes =
              if (xmlReader.getNamespacePrefix(i) == null) {
                val uri = xmlReader.getNamespaceURI(i)
                scopes.append(if(uri == null) "" else uri)
              }
              else
                scopes.append(prefix, xmlReader.getNamespaceURI(i))
            i = i + 1
          }
//          println("scopes=" + scopes)
//          var prefixes = prefixMapping.headOption getOrElse Map()
//          while (i < xmlReader.getNamespaceCount) {
//            val ns = xmlReader.getNamespaceURI(i)
//            val rawPrefix = xmlReader.getNamespacePrefix(i)
//            val prefix = if (rawPrefix != null) rawPrefix else ""
//
//            // To conserve memory, only save prefix if changed
//            if (prefixes.get(prefix) != Some(ns)) {
//              prefixes = prefixes + (prefix -> ns)
//            }
//            i = i + 1
//          }
//          prefixMapping ::= prefixes
          i = 0
          var attrs = Attributes()
          while (i < xmlReader.getAttributeCount) {
            val localName = xmlReader.getAttributeLocalName(i)
            val prefix = {
              val back = xmlReader.getAttributePrefix(i)
              if (back == null || back == "") None else Some(back)
            }
            attrs = attrs + (QName(prefix, localName) -> xmlReader.getAttributeValue(i))
            i = i + 1
          }
          val namespace =
            if(xmlReader.getPrefix.isEmpty) {
              val uri = xmlReader.getNamespaceURI
              if(uri == null)
                NamespaceBinding.empty
              else
                NamespaceBinding(uri)
            }
            else
              NamespaceBinding(xmlReader.getPrefix, xmlReader.getNamespaceURI)
          elems ::= ElemBuilder(xmlReader.getLocalName, scopes, attrs)
           results ::= VectorCase.newBuilder[Node]           
        }
        case _ =>
      }
    }
    results.head.result().head.asInstanceOf[Elem]
  }
}
