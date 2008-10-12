package net.lag.naggati.asn1

import net.lag.extensions._
import net.lag.naggati.ProtocolError
import net.lag.naggati.Steps._


object Tag {
  abstract sealed case class Domain(value: Int)
  case object UNIVERSAL extends Domain(0)
  case object APPLICATION extends Domain(1)
  case object CONTEXT extends Domain(2)
  case object PRIVATE extends Domain(3)

  def domainFor(n: Int) = n match {
    case 0 => UNIVERSAL
    case 1 => APPLICATION
    case 2 => CONTEXT
    case 3 => PRIVATE
  }

  val TERMINATOR = Tag.create(UNIVERSAL, 0, 0)

  def create(domain: Domain, kind: Int) = new Tag(domain, kind, 0, false, false)
  def create(domain: Domain, kind: Int, size: Int) = new Tag(domain, kind, size, false, true)
  def createContainer(domain: Domain, kind: Int) = new Tag(domain, kind, 0, true, false)
  def createContainer(domain: Domain, kind: Int, size: Int) = new Tag(domain, kind, size, true, true)

  /**
   * Read an ASN.1 BER tag and write it into the state table under the given
   * name.
   */
  def reader(key: String) = readInt8 { t =>
    val domain: Int = (t >> 6) & 0x03
    val isContainer = (t & 0x20) != 0
    var kind: Int = t & 0x1f
    readUntil(b => (kind != 0x1f) || ((b & 0x80) != 0x80)) { n =>
      if (kind == 0x1f) {
        /* extended form of tag.
         * in theory, this tag could be an infinite number of bits long.
         * for our simple implementation, we restrict to 28 bits.  that
         * limits us to 256M tags per domain, boo hoo.
         */
        if (n > 4) {
          throw new ProtocolError("ASN.1 Tag kind is too long for this library (> 256M)")
        }
        kind = 0
        for (i <- 0 until n) { kind = (kind << 7) | (state.buffer.get & 0x7f) }
      }

      // now read the size.
      readInt8 { t =>
        if (t == 0x80) {
          // indefinite size.
          if (! isContainer) {
            throw new ProtocolError("ASN.1 Tag is indefinite-size, non-container (illegal)")
          }
          state(key) = new Tag(domainFor(domain), kind, 0, isContainer, false)
          End
        } else if ((t & 0x80) == 0x80) {
          /* extended size field.
           * in theory, the size field can be an infinite number of bits
           * long. for our simple implementation, we restrict this to 31
           * bits, or 2GB for a single ASN.1 element.
           */
          if ((t & 0x7f) > 4) {
            throw new ProtocolError("ASN.1 Tag size is too long for this library (> 2G)")
          }
          readByteBuffer(t & 0x7f) { buffer =>
            state(key) = new Tag(domainFor(domain), kind, BigInt(1, buffer).intValue, isContainer, true)
            End
          }
        } else {
          state(key) = new Tag(domainFor(domain), kind, t, isContainer, true)
          End
        }
      }
    }
  }
}


class Tag private(val domain: Tag.Domain, val kind: Int, val size: Int, val isContainer: Boolean,
  val hasSize: Boolean) {

  override def toString: String = {
    "<ASN.1 Tag(%s, %d, size=%s, container=%s)>".format(domain, kind,
      if (hasSize) Integer.toString(size) else "indefinite",
      if (isContainer) "yes" else "no")
  }

  override def hashCode = {
    (domain.value * 37 + kind) * 37 + (if (isContainer) 1 else 0)
  }

  override def equals(other: Any) = {
    if (! other.isInstanceOf[Tag]) {
      false
    } else {
      val otherTag = other.asInstanceOf[Tag]
      (this.domain == otherTag.domain) && (this.kind == otherTag.kind) && (this.isContainer == otherTag.isContainer)
    }
  }

  /**
   * Return a tag of the same domain & type as this one, but with a
   * different value in the size field. This is useful for making a
   * prototype Tag with all the fields specified, and using it to generate
   * tags of various sizes for writing to streams.
   */
  def asSize(size: Int) = {
    new Tag(this.domain, this.kind, size, this.isContainer, true)
  }

}
