/*
 * Copyright 2008-2014 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody;

import java.awt.datatransfer.DataFlavor;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.util.Locale;
import java.util.Map;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.collections.MapConverter;
import com.thoughtworks.xstream.io.json.JsonHierarchicalStreamDriver;
import com.thoughtworks.xstream.io.xml.CompactWriter;

/**
 * Liste des formats de transport entre un serveur de collecte et une application monitorée
 * (hors protocole à priori http).
 * @author Emeric Vernat
 */
enum TransportFormat {
	/**
	 * Sérialisation java.
	 */
	SERIALIZED(DataFlavor.javaSerializedObjectMimeType),

	/**
	 * XML (avec XStream/XPP).
	 * <br/>Selon http://code.google.com/p/thrift-protobuf-compare/wiki/Benchmarking?ts=1237772203&updated=Benchmarking,
	 * la sérialisation java est 75% plus performante en temps que xml (xstream/xpp)
	 * et à peine plus gourmande en taille de flux.
	 */
	XML("text/xml; charset=utf-8"),

	/**
	 * JSON (écriture en JSON avec XStream).
	 * Note : il serait possible aussi de le faire avec Jackson (http://jackson.codehaus.org/)
	 */
	JSON("application/json");

	private static final String NULL_VALUE = "null";

	// classe interne pour qu'elle ne soit pas chargée avec la classe TransportFormat
	// et qu'ainsi on ne dépende pas de XStream si on ne se sert pas du format xml
	private static final class XmlIO {
		private static final String XML_CHARSET_NAME = "utf-8";

		private XmlIO() {
			super();
		}

		static void writeToXml(Serializable serializable, BufferedOutputStream bufferedOutput)
				throws IOException {
			final XStream xstream = createXStream(false);
			// on wrappe avec un CompactWriter pour gagner 25% en taille de flux (retours chariots)
			// et donc un peu en performances
			final CompactWriter writer = new CompactWriter(new OutputStreamWriter(bufferedOutput,
					XML_CHARSET_NAME));
			try {
				xstream.marshal(serializable, writer);
			} finally {
				writer.close();
			}
		}

		static Object readFromXml(InputStream bufferedInput) throws IOException {
			final XStream xstream = createXStream(false);
			final InputStreamReader reader = new InputStreamReader(bufferedInput, XML_CHARSET_NAME);
			try {
				return xstream.fromXML(reader);
			} finally {
				reader.close();
			}
		}

		static void writeToJson(Serializable serializable, BufferedOutputStream bufferedOutput)
				throws IOException {
			final XStream xstream = createXStream(true);
			try {
				xstream.toXML(serializable, bufferedOutput);
			} finally {
				bufferedOutput.close();
			}
		}

		private static XStream createXStream(boolean json) {
			final XStream xstream;
			if (json) {
				// format json
				xstream = new XStream(new JsonHierarchicalStreamDriver());
				xstream.setMode(XStream.NO_REFERENCES);
			} else {
				// sinon format xml, utilise la dépendance XPP3 par défaut
				xstream = new XStream();
			}
			for (final Map.Entry<String, Class<?>> entry : XStreamAlias.getMap().entrySet()) {
				xstream.alias(entry.getKey(), entry.getValue());
			}
			final MapConverter mapConverter = new MapConverter(xstream.getMapper()) {
				/** {@inheritDoc} */
				@SuppressWarnings("rawtypes")
				@Override
				public boolean canConvert(Class type) {
					return true; // Counter.requests est bien une map
				}
			};
			xstream.registerLocalConverter(Counter.class, "requests", mapConverter);
			xstream.registerLocalConverter(Counter.class, "rootCurrentContextsByThreadId",
					mapConverter);
			return xstream;
		}
	}

	private final String code; // NOPMD
	private final String mimeType; // NOPMD

	private TransportFormat(String mimeType) {
		this.mimeType = mimeType;
		this.code = this.toString().toLowerCase(Locale.ENGLISH);
	}

	static TransportFormat valueOfIgnoreCase(String transportFormat) {
		return valueOf(transportFormat.toUpperCase(Locale.ENGLISH).trim());
	}

	static boolean isATransportFormat(String format) {
		if (format == null) {
			return false;
		}
		final String upperCase = format.toUpperCase(Locale.ENGLISH).trim();
		for (final TransportFormat transportFormat : TransportFormat.values()) {
			if (transportFormat.toString().equals(upperCase)) {
				return true;
			}
		}
		return false;
	}

	static void pump(InputStream input, OutputStream output) throws IOException {
		final byte[] bytes = new byte[4 * 1024];
		int length = input.read(bytes);
		while (length != -1) {
			output.write(bytes, 0, length);
			length = input.read(bytes);
		}
	}

	String getCode() {
		return code;
	}

	String getMimeType() {
		return mimeType;
	}

	void checkDependencies() throws IOException {
		if (this == XML || this == JSON) {
			try {
				Class.forName("com.thoughtworks.xstream.XStream");
			} catch (final ClassNotFoundException e) {
				throw new IOException(
						"Classes of the XStream library not found. Add the XStream dependency in your webapp for the XML or JSON formats.",
						e);
			}
		}
		if (this == XML) {
			try {
				Class.forName("org.xmlpull.v1.XmlPullParser");
			} catch (final ClassNotFoundException e) {
				throw new IOException(
						"Classes of the XPP3 library not found. Add the XPP3 dependency in your webapp for the XML format.",
						e);
			}
		}
	}

	void writeSerializableTo(Serializable serializable, OutputStream output) throws IOException {
		final Serializable nonNullSerializable;
		if (serializable == null) {
			nonNullSerializable = NULL_VALUE;
		} else {
			nonNullSerializable = serializable;
		}
		final BufferedOutputStream bufferedOutput = new BufferedOutputStream(output);
		switch (this) {
		case SERIALIZED:
			final ObjectOutputStream out = new ObjectOutputStream(bufferedOutput);
			try {
				out.writeObject(nonNullSerializable);
			} finally {
				out.close();
			}
			break;
		case XML:
			// Rq : sans xstream et si jdk 1.6, on pourrait sinon utiliser
			// XMLStreamWriter et XMLStreamReader ou JAXB avec des annotations
			XmlIO.writeToXml(nonNullSerializable, bufferedOutput);
			break;
		case JSON:
			XmlIO.writeToJson(nonNullSerializable, bufferedOutput);
			break;
		default:
			throw new IllegalStateException(toString());
		}
	}

	Serializable readSerializableFrom(InputStream input) throws IOException, ClassNotFoundException {
		final InputStream bufferedInput = new BufferedInputStream(input);
		Object result;
		switch (this) {
		case SERIALIZED:
			final ObjectInputStream in = new ObjectInputStream(bufferedInput);
			try {
				result = in.readObject();
			} finally {
				in.close();
			}
			break;
		case XML:
			result = XmlIO.readFromXml(bufferedInput);
			break;
		case JSON:
			// pas possible avec JsonHierarchicalStreamDriver
			// (http://xstream.codehaus.org/json-tutorial.html)
			throw new UnsupportedOperationException();
		default:
			throw new IllegalStateException(toString());
		}
		if (NULL_VALUE.equals(result)) {
			result = null;
		}
		// c'est un Serializable que l'on a écrit
		return (Serializable) result;
	}
}
