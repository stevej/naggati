/*
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.naggati.asn1

import net.lag.naggati.Steps._
import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.filterchain.IoFilter
import org.apache.mina.core.session.{DummySession, IoSession}
import org.apache.mina.filter.codec.ProtocolDecoderOutput
import org.specs._


object TagSpec extends Specification {

  private var fakeSession: IoSession = null
  private var fakeDecoderOutput: ProtocolDecoderOutput = null

  def unhexlify(s: String): Array[Byte] = {
    val buffer = new Array[Byte](s.length / 2)
    for (val i <- 0.until(s.length, 2)) {
      buffer(i/2) = Integer.parseInt(s.substring(i, i+2), 16).toByte
    }
    buffer
  }


  private class TagDecoder extends Decoder(Tag.reader("out")) {
    val fakeSession = new DummySession
    val fakeDecoderOutput = new ProtocolDecoderOutput {
      override def flush(nextFilter: IoFilter.NextFilter, s: IoSession) = {}
      override def write(obj: AnyRef) = {}
    }

    def quickDecode(s: String): Unit = {
      decode(fakeSession, IoBuffer.wrap(unhexlify(s)), fakeDecoderOutput)
    }

    def getTag: Option[Tag] = {
      stateFor(fakeSession, fakeDecoderOutput).data.get("out") match {
        case None => None
        case Some(x) => Some(x.asInstanceOf[Tag])
      }
    }
  }


  "ASN.1 Tag" should {
    "decode simple tags" in {
      val decoder = new TagDecoder
      decoder.getTag mustEqual None
      decoder.quickDecode("0000")
      decoder.getTag mustEqual Some(Tag.create(Tag.UNIVERSAL, 0, 0))
      decoder.quickDecode("7E00")
      decoder.getTag mustEqual Some(Tag.createContainer(Tag.APPLICATION, 30, 0))
      decoder.quickDecode("8100")
      decoder.getTag mustEqual Some(Tag.create(Tag.CONTEXT, 1, 0))
      decoder.quickDecode("C900")
      decoder.getTag mustEqual Some(Tag.create(Tag.PRIVATE, 9, 0))
    }

    "decode long kind" in {
      val decoder = new TagDecoder
      decoder.getTag mustEqual None
      decoder.quickDecode("9F822301")
      decoder.getTag mustEqual Some(Tag.create(Tag.CONTEXT, 0x123, 1))
      decoder.quickDecode("9FE5BFD76E00")
      decoder.getTag mustEqual Some(Tag.create(Tag.CONTEXT, 0xcafebee, 1))
    }

    "decode long size" in {
      val decoder = new TagDecoder
      decoder.getTag mustEqual None
      decoder.quickDecode("C0820103")
      decoder.getTag mustEqual Some(Tag.create(Tag.PRIVATE, 0, 259))
      decoder.quickDecode("308181")
      decoder.getTag mustEqual Some(Tag.createContainer(Tag.UNIVERSAL, 16, 129))
    }

    "decode long kind and size" in {
      val decoder = new TagDecoder
      decoder.getTag mustEqual None
      decoder.quickDecode("1F866682FFFE")
      decoder.getTag mustEqual Some(Tag.create(Tag.UNIVERSAL, 0x366, 65534))
    }

    "catch errors" in {
      // tag type too long
      val decoder = new TagDecoder
      decoder.quickDecode("9F8CD7FAF53E00") must throwA[ProtocolError]
      // size field too long
      val decoder2 = new TagDecoder
      decoder2.quickDecode("C0850000000000") must throwA[ProtocolError]
    }
  }
}
