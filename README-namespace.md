# Improved Namespace Support for Anti-XML

These are my notes on improving the namespace support for anti-xml.

## Ideas/thoughs

### Consistent naming

Rename "scope" to "namespace", ensure that the naming and concepts are closer to the XML specs.

### Selectors and namespaces

Selecting a child nodes with `\`, should it require the child elements to be in the same namespace as the element it's
applied on?

I think it should as that'll give a consumer the least number of surprises. When doing the xpath-like lookups you're
already in a "lax mode" and only picking out stuff that you know and ignoring the rest.

### Make NamespaceBinding look like a linked list

In XML elements might override namespace prefixes, for example:

    <!--
      Element namespace: "urn:a"
      Namespace bindings: "urn:a"
     -->
    <root xmlns="urn:a">
      <a>
        <!--
          Element namespace: "urn:person"
          Namespace bindings: "urn:person", "urn:a"
        -->
        <person xmlns="urn:person">
          <!--
            Element namespace: "urn:person"
            Namespace bindings: "urn:person", "urn:a"
          -->
          <name>Captain Awesome</name>
        </person>
      </a>
      <!--
        Element namespace: "urn:person"
        Namespace bindings: person="urn:person", "urn:a"

        This is the same element as the above <persion/>
      -->
      <person:person xmlns:person="urn:person">
        <!--
          Element namespace: "urn:person"
          Namespace bindings: "urn:person", "urn:a"
        -->
        <person:name>Captain Awesome</name>
      </person:person>
    </root>

Another example:

    <!--
      Element namespace: None
      Namespace bindings: None
     -->
    <root>
      <!--
        Element namespace: "urn:person"
        Namespace bindings: "urn:person", "urn:a"
      -->
      <person xmlns="urn:person">
        <!--
          Element namespace: "urn:person"
          Namespace bindings: "urn:person", "urn:a"
        -->
        <name>Captain Awesome</name>
      </person>
    </root>

With these example it's easy to see that the `NamespaceBinding` objects can be easily share between `Elem` objects and
will give a rather conservative memory usage as child bindings just reference a parent binding.

### Namespace collisions

TODO: this really isn't a problem as far as I can tell - trygve

As it is now there's nothing that prevent namespace collisions. If there's a generic piece of code that generate a
"person" `Elem` inside the "urn:person" namespace and another piece of code that generate a "car" `Elem` stuff will
get complicated:

    <root>
      <person xmlns="urn:person"/>
      <car xmlns="urn:car"/>
    </root>

### Reuse specs for sax and stax parser

Right now they're almost the same but both have some extra tests which apply to the other parser too. I don't know how
to do that best with specs, but there's got to be a clever way.

## TODO

* Use the same concepts for namespacing attributes as elements.
* Update documentation and examples.

## Completed

### Make anti-xml be namespace-oriented, not prefix-oriented

* Added a "NamespaceBinding" class which contain the current bindings. Similar to scala.xml's bindings.
* The "scope" field is not an Option, however it can reference the empty namespace binding. This is mainly a memory
  optimalization so that the bindings doesn't have to be wrapped in an Option and unwrapped all the time when
  pattern matching them.
