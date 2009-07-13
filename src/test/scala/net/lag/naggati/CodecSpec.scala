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

package net.lag.naggati

import net.lag.extensions._
import net.lag.naggati.Steps._
import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.filterchain.IoFilter
import org.apache.mina.core.session.{DummySession, IoSession}
import org.apache.mina.filter.codec._
import org.specs._
import java.nio.ByteOrder


object CodecSpec extends Specification {

  private var fakeSession: IoSession = null
  private var fakeDecoderOutput: ProtocolDecoderOutput = null
  private var written: List[AnyRef] = Nil

  def quickDecode(decoder: Decoder, s: String): Unit = quickDecode(decoder, s.getBytes)
  def quickDecode(decoder: Decoder, b: Array[Byte]): Unit = quickDecode(decoder, IoBuffer.wrap(b))
  def quickDecode(decoder: Decoder, buf: IoBuffer): Unit = {
    decoder.decode(fakeSession, buf, fakeDecoderOutput)
  }


  "Decoder" should {
    doBefore {
      written = Nil
      fakeSession = new DummySession
      fakeDecoderOutput = new ProtocolDecoderOutput {
        override def flush(nextFilter: IoFilter.NextFilter, s: IoSession) = {}
        override def write(obj: AnyRef) = {
          written = written ++ List(obj)
        }
      }
    }


    "read a fixed number of bytes" in {
      var scored = false
      val step = readByteBuffer(3) { buffer =>
        scored = true
        End
      }

      val decoder = new TestDecoder(step)
      decoder("xx") mustEqual Nil
      scored mustEqual false
      decoder("y") mustEqual Nil
      scored mustEqual true
    }


    // FIXME: convert these other tests to use TestDecoder too.

    "read a variable number of bytes" in {
      var n = 2
      var rv: List[String] = Nil
      val step = readBytes(n) {
        val byteBuffer = new Array[Byte](n)
        state.buffer.get(byteBuffer)
        rv = new String(byteBuffer) :: rv
        n += 1
        End
      }
      val decoder = new Decoder(step)

      quickDecode(decoder, "abcde")
      n mustEqual 4
      rv mustEqual List("cde", "ab")
    }

    "read a fixed number of bytes, in chunks" in {
      // chunk up every 4 bytes:
      val step = readByteBuffer(4) { buffer =>
        state.out.write(new String(buffer))
        End
      }
      val decoder = new Decoder(step)

      // partial write gives nothing:
      decoder.decode(fakeSession, IoBuffer.wrap("12".getBytes), fakeDecoderOutput)
      written mustEqual Nil
      // overlap write finishes one block and continues buffering:
      decoder.decode(fakeSession, IoBuffer.wrap("345".getBytes), fakeDecoderOutput)
      written mustEqual List("1234")
      // overlap write continues to block correctly:
      decoder.decode(fakeSession, IoBuffer.wrap("6789".getBytes), fakeDecoderOutput)
      written mustEqual List("1234", "5678")
      // many-block write gives all finished blocks:
      decoder.decode(fakeSession, IoBuffer.wrap("ABCDEFGHIJKLM".getBytes), fakeDecoderOutput)
      written mustEqual List("1234", "5678", "9ABC", "DEFG", "HIJK")
      // partial write gives nothing, even with partial buffer:
      decoder.decode(fakeSession, IoBuffer.wrap("N".getBytes), fakeDecoderOutput)
      written mustEqual List("1234", "5678", "9ABC", "DEFG", "HIJK")
      // exact block closing gives a block:
      decoder.decode(fakeSession, IoBuffer.wrap("O".getBytes), fakeDecoderOutput)
      written mustEqual List("1234", "5678", "9ABC", "DEFG", "HIJK", "LMNO")
      // ditto for an exact whole block:
      decoder.decode(fakeSession, IoBuffer.wrap("PQRS".getBytes), fakeDecoderOutput)
      written mustEqual List("1234", "5678", "9ABC", "DEFG", "HIJK", "LMNO", "PQRS")
    }

    "read up to a delimiter" in {
      var scored = false
      val step = readDelimiterBuffer('\n'.toByte) { buffer =>
        scored = true
        End
      }
      val decoder = new Decoder(step)
      decoder.decode(fakeSession, IoBuffer.wrap("hello".getBytes), fakeDecoderOutput)
      written mustEqual Nil
      scored mustEqual false
      decoder.decode(fakeSession, IoBuffer.wrap(" kitty\n".getBytes), fakeDecoderOutput)
      written mustEqual Nil
      scored mustEqual true
    }

    "read up to a multi-byte delimiter" in {
      var found: List[String] = Nil
      val step = readDelimiterBuffer("cat".getBytes) { buffer =>
        found = new String(buffer) :: found
        End
      }
      val decoder = new Decoder(step)

      quickDecode(decoder, "quickly the catapult sang.")
      found mustEqual List("quickly the cat")
      quickDecode(decoder, " then the cat jumped on cathy.")
      found mustEqual List(" jumped on cat", "apult sang. then the cat", "quickly the cat")
    }

    "read up to a delimiter, in chunks" in {
      val step = readDelimiterBuffer('\n'.toByte) { buffer =>
        state.out.write(new String(buffer))
        End
      }
      val decoder = new Decoder(step)

      // partial write gives nothing:
      quickDecode(decoder, "partia")
      written mustEqual Nil
      // overlap write finishes one block and continues buffering:
      quickDecode(decoder, "l\nand")
      written mustEqual List("partial\n")
      // overlap write continues to block correctly:
      quickDecode(decoder, " another\nbut the")
      written mustEqual List("partial\n", "and another\n")
      // many-block write gives all finished blocks:
      quickDecode(decoder, "n\nmany\nnew ones\nbo")
      written mustEqual List("partial\n", "and another\n", "but then\n", "many\n", "new ones\n")
      // partial write gives nothing, even with partial buffer:
      quickDecode(decoder, "re")
      written mustEqual List("partial\n", "and another\n", "but then\n", "many\n", "new ones\n")
      // exact block closing gives a block:
      quickDecode(decoder, "d now\n")
      written mustEqual List("partial\n", "and another\n", "but then\n", "many\n", "new ones\n", "bored now\n")
      // ditto for an exact whole block:
      quickDecode(decoder, "bye\n")
      written mustEqual List("partial\n", "and another\n", "but then\n", "many\n", "new ones\n", "bored now\n", "bye\n")
    }

    "read a line" in {
      val step = readLine { line => state.out.write(line); End }
      val decoder = new Decoder(step)

      quickDecode(decoder, "hello there\r\ncat")
      written mustEqual List("hello there")
      quickDecode(decoder,"s don't use CR\n")
      written mustEqual List("hello there", "cats don't use CR")
      quickDecode(decoder, "thing\r\n\nstop\r\n\r\nokay.\n")
      written mustEqual List("hello there", "cats don't use CR", "thing", "", "stop", "", "okay.")
    }

    "read a line, preserving CRLF" in {
      val step = readLine(false) { line => state.out.write(line); End }
      val decoder = new Decoder(step)

      quickDecode(decoder, "hello there\r\ncat")
      written mustEqual List("hello there\r\n")
      quickDecode(decoder, "s don't use CR\n")
      written mustEqual List("hello there\r\n", "cats don't use CR\n")
      quickDecode(decoder, "thing\r\n\nstop\r\n\r\nokay.\n")
      written mustEqual List("hello there\r\n", "cats don't use CR\n", "thing\r\n", "\n", "stop\r\n", "\r\n", "okay.\n")
    }

    "read until" in {
      var out = -1
      val step = readUntil(b => (b & 0x01) == 0x01) { n =>
        out = n
        state.buffer.position(state.buffer.position + n)
        End
      }
      val decoder = new Decoder(step)
      quickDecode(decoder, "bdd")
      out mustEqual -1
      quickDecode(decoder, "bad")
      out mustEqual 5
      quickDecode(decoder, "e")
      out mustEqual 2
    }

    "combine reading modes" in {
      val step = readInt32 { len =>
        readByteBuffer(len) { bytes =>
          state.out.write(new String(bytes, "UTF-8"))
          End
        }
      }
      val decoder = new Decoder(step)

      val buffer = IoBuffer.allocate(9)
      buffer.order(ByteOrder.BIG_ENDIAN)
      buffer.putInt(5)
      buffer.put("hello".getBytes)
      buffer.flip
      quickDecode(decoder, buffer)
      written mustEqual List("hello")
    }

    "combine reading modes with branching" in {
      // 1-byte "type" field indicates if a string or int follows
      val step = readInt8 { datatype =>
        if ((datatype & 0x80) == 0) {
          readByteBuffer(datatype & 0x7f) { bytes =>
            state.out.write(new String(bytes, "UTF-8"))
            End
          }
        } else {
          readInt32 { n =>
            state.out.write(n)
            End
          }
        }
      }
      val decoder = new Decoder(step)

      val buffer = IoBuffer.allocate(14)
      buffer.order(ByteOrder.BIG_ENDIAN)
      buffer.put(3.toByte)
      buffer.put("cat".getBytes)
      buffer.put(0xff.toByte)
      buffer.putInt(23)
      buffer.put(4.toByte)
      buffer.put("yay!".getBytes)
      buffer.flip
      quickDecode(decoder, buffer)
      written mustEqual List("cat", 23, "yay!")
    }

    "compose readByteBuffer" in {
      // 1-byte "type" field indicates if a string or int follows
      val step = readByteBuffer(1) { buffer =>
        val datatype = buffer(0)
        if ((datatype & 0x80) == 0) {
          readByteBuffer(datatype & 0x7f) { bytes =>
            state.out.write(new String(bytes, "UTF-8"))
            End
          }
        } else {
          readInt32 { n =>
            state.out.write(n)
            End
          }
        }
      }
      val decoder = new Decoder(step)

      val buffer = IoBuffer.allocate(14)
      buffer.order(ByteOrder.BIG_ENDIAN)
      buffer.put(3.toByte)
      buffer.put("cat".getBytes)
      buffer.put(0xff.toByte)
      buffer.putInt(23)
      buffer.put(4.toByte)
      buffer.put("yay!".getBytes)
      buffer.flip
      quickDecode(decoder, buffer)
      written mustEqual List("cat", 23, "yay!")
    }

    "readByteBuffer composes with readDelimiter and readByteBuffer" in {
      val step = readByteBuffer(12) { buffer =>
        state.out.write(buffer)
        readDelimiterBuffer(0x00.toByte) { buffer =>
          state.out.write(buffer)
          readByteBuffer(2) { buffer =>
            state.out.write(buffer)
            readByteBuffer(2) { buffer =>
              state.out.write(buffer)
              End
            }
          }
        }
      }

      val decoder = new Decoder(step)
      decoder.decode(fakeSession, IoBuffer.wrap(Array(0xed, 0x82, 0x01, 0x00, 0x00, 0x01, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x03, 0x63, 0x6f, 0x6d,
        0x00, 0x00, 0x01, 0x00, 0x01).map(_.toByte)), fakeDecoderOutput)
      written(0).asInstanceOf[Array[Byte]].hexlify mustEqual Array(0xed, 0x82, 0x01, 0x00, 0x00, 0x01, 0x00,
          0x00, 0x00, 0x00, 0x00, 0x00).map { _.toByte }.hexlify
      written(1).asInstanceOf[Array[Byte]].hexlify mustEqual Array(0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x03, 0x63, 0x6f, 0x6d,
          0x00).map { _.toByte }.hexlify
      written(2).asInstanceOf[Array[Byte]].hexlify mustEqual Array(0x00, 0x01).map { _.toByte }.hexlify
      written(3).asInstanceOf[Array[Byte]].hexlify mustEqual Array(0x00, 0x01).map { _.toByte }.hexlify
    }

    "chain 3 implicit steps together" in {
      var list: List[String] = Nil
      val step1 = step { list = "a" :: list; COMPLETE }
      val step2 = step { list = "b" :: list; COMPLETE }
      val step3 = step { list = "c" :: list; NEED_DATA }
      val x = step1 :: step2 :: step3
      val decoder = new Decoder(x)
      val buffer = IoBuffer.allocate(1)
      quickDecode(decoder, buffer)
      list mustEqual List("c", "b", "a")
    }

    "handle being called multiple times with the same buffer (mina 2.0-M4)" in {
      val step = readLine { line => state.out.write(line); End }
      val decoder = new Decoder(step)

      val buffer = IoBuffer.wrap("another line.\r\n".getBytes)
      decoder.decode(fakeSession, buffer, fakeDecoderOutput)
      decoder.decode(fakeSession, buffer, fakeDecoderOutput)
      written mustEqual List("another line.")
    }
  }
}
