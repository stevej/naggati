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

import net.lag.naggati.Steps._
import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.filterchain.IoFilter
import org.apache.mina.core.session.{DummySession, IoSession}
import org.apache.mina.filter.codec.ProtocolDecoderOutput
import org.specs._
import scala.collection.mutable


case class RequestLine(method: String, resource: String, version: String)
case class HeaderLine(name: String, value: String)
case class HttpRequest(request: RequestLine, headers: List[HeaderLine], body: Array[Byte])

object HttpRequestDecoder {
  def readRequest = {
    readLine(true, "UTF-8") { line =>
      line.split(' ').toList match {
        case method :: resource :: version :: Nil =>
          state("request") = RequestLine(method, resource, version)
          state("headers") = new mutable.ListBuffer[HeaderLine]
          readHeader
        case _ => throw new ProtocolError("Malformed request line: " + line)
      }
    }
  }

  val readHeader: Step = readLine(true, "UTF-8") { line =>
    val headers: mutable.ListBuffer[HeaderLine] = state("headers")
    if (line == "") {
      // end of headers
      val contentLength = headers find { _.name == "content-length" } map { _.value.toInt } getOrElse 0
      readByteBuffer(contentLength) { data =>
        state.out.write(new HttpRequest(state("request"), headers.toList, data))
        End
      }
    } else if (line.length > 0 && (line.first == ' ' || line.first == '\t')) {
      // continuation line
      if (headers.size == 0) {
        throw new ProtocolError("Malformed header line: " + line)
      }
      val newHeaderLine = HeaderLine(headers.last.name, headers.last.value + " " + line.trim)
      headers.trimEnd(1)
      headers += newHeaderLine
      readHeader
    } else {
      line.split(':').toList match {
        case name :: value :: Nil => headers += HeaderLine(name.trim.toLowerCase, value.trim)
        case _ => throw new ProtocolError("Malformed header line: " + line)
      }
      readHeader
    }
  }
}


object HttpRequestSpec extends Specification {

  private var fakeSession: IoSession = null
  private var fakeDecoderOutput: ProtocolDecoderOutput = null
  private var written = new mutable.ListBuffer[HttpRequest]

  def quickDecode(decoder: Decoder, s: String): Unit = {
    decoder.decode(fakeSession, IoBuffer.wrap(s.getBytes), fakeDecoderOutput)
  }

  val TEST1 = """POST /query.cgi HTTP/1.1
Host: www.example.com
Content-Length: 6
X-Special-Request: cheese
 and ham

hello!"""

  "HttpRequestDecoder" should {
    doBefore {
      written.clear()
      fakeSession = new DummySession
      fakeDecoderOutput = new ProtocolDecoderOutput {
        override def flush(nextFilter: IoFilter.NextFilter, s: IoSession) = {}
        override def write(obj: AnyRef) = written += obj.asInstanceOf[HttpRequest]
      }
    }

    "parse a simple http request" in {
      val decoder = new Decoder(HttpRequestDecoder.readRequest)
      quickDecode(decoder, TEST1)

      written.size mustEqual 1
      written(0).request mustEqual RequestLine("POST", "/query.cgi", "HTTP/1.1")
      written(0).headers.size mustEqual 3
      written(0).headers(0) mustEqual HeaderLine("host", "www.example.com")
      written(0).headers(1) mustEqual HeaderLine("content-length", "6")
      written(0).headers(2) mustEqual HeaderLine("x-special-request", "cheese and ham")
      new String(written(0).body) mustEqual "hello!"
    }
  }
}
