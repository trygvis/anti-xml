package com.codecommit.antixml

import org.specs2.mutable._

class NamespaceSpecs extends Specification {
  val urnA = NamespaceBinding("urn:a")
  val urnB = NamespaceBinding("urn:b")

  "findByUri" should {
    "find None on empty" in {
      NamespaceBinding.empty.findByUri("urn:a") must beNone
    }

    "find Some('urn:a') from 'urn:a'" in {
      urnA.findByUri("urn:a") must beSome(urnA)
    }

    "find Some('urn:a') from 'urn:a' :: 'urn:b'" in {
      NamespaceBinding("urn:b", urnA).findByUri("urn:a") must beSome(urnA)
    }

    "find None from NamespaceBinding('urn:a')" in {
      urnA.findByUri("urn:b") must beNone
    }
  }
}
