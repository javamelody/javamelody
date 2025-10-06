/*
 * Copyright 2008-2019 by Emeric Vernat
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
package net.bull.javamelody.internal.model;

import java.awt.datatransfer.DataFlavor;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Locale;
import java.util.Map;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializer;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.converters.collections.CollectionConverter;
import com.thoughtworks.xstream.converters.collections.MapConverter;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.json.JsonHierarchicalStreamDriver;
import com.thoughtworks.xstream.io.xml.CompactWriter;
import com.thoughtworks.xstream.security.NoTypePermission;
import com.thoughtworks.xstream.security.NullPermission;
import com.thoughtworks.xstream.security.PrimitiveTypePermission;

/**
 * Liste des formats de transport entre un serveur de collecte et une application monitorée
 * (hors protocole à priori http).
 * @author Emeric Vernat
 */
public enum TransportFormat {
	/**
	 * Sérialisation java.
	 */
	SERIALIZED(DataFlavor.javaSerializedObjectMimeType),

	/**
	 * XML (avec <a href='http://x-stream.github.io/'>XStream</a> / XPP).
	 */
	XML("text/xml; charset=utf-8"),

	/**
	 * JSON (écriture en JSON avec <a href='http://x-stream.github.io/'>XStream</a>).
	 * Note : il serait possible aussi de le faire avec <a href='https://github.com/FasterXML/jackson'>Jackson</a>
	 */
	JSON("application/json"),

	/**
	 * GSON (écriture et lecture en JSON avec <a href='https://github.com/google/gson'>Google Gson</a>).
	 * Note : il serait possible aussi de le faire avec <a href='https://github.com/FasterXML/jackson'>Jackson</a>
	 */
	GSON("application/json");

	private static final String NULL_VALUE = "null";

	// classe interne pour qu'elle ne soit pas chargée avec la classe TransportFormat
	// et qu'ainsi on ne dépende pas de XStream si on ne se sert pas du format xml
	private static final class XmlIO {
		private static final String PACKAGE_NAME = TransportFormat.class.getName().substring(0,
				TransportFormat.class.getName().length()
						- TransportFormat.class.getSimpleName().length() - 1);
		private static final String XML_CHARSET_NAME = "utf-8";

		private XmlIO() {
			super();
		}

		static void writeToXml(Serializable serializable, BufferedOutputStream bufferedOutput)
				throws IOException {
			final XStream xstream = createXStream(false);
			// on wrappe avec un CompactWriter pour gagner 25% en taille de flux (retours chariots)
			// et donc un peu en performances
			final CompactWriter writer = new CompactWriter(
					new OutputStreamWriter(bufferedOutput, XML_CHARSET_NAME));
			try {
				xstream.marshal(serializable, writer);
			} finally {
				writer.close();
			}
		}

		static Object readFromXml(InputStream bufferedInput) throws IOException {
			final XStream xstream = createXStream(false);
			// see http://x-stream.github.io/security.html
			// clear out existing permissions and set own ones
			xstream.addPermission(NoTypePermission.NONE);
			// allow some basics
			xstream.addPermission(NullPermission.NULL);
			xstream.addPermission(PrimitiveTypePermission.PRIMITIVES);
			xstream.allowTypesByWildcard(
					new String[] { "java.lang.*", "java.util.*", "java.util.concurrent.*" });
			// allow any type from the same package
			xstream.allowTypesByWildcard(new String[] { PACKAGE_NAME + ".*" });
			try (InputStreamReader reader = new InputStreamReader(bufferedInput,
					XML_CHARSET_NAME)) {
				return xstream.fromXML(reader);
			}
		}

		static void writeToJson(Serializable serializable, BufferedOutputStream bufferedOutput)
				throws IOException {
			final XStream xstream = createXStream(true);
			try (bufferedOutput) {
				xstream.toXML(serializable, bufferedOutput);
			}
		}

		private static XStream createXStream(boolean json) {
			final XStream xstream;
			if (json) {
				// format json
				xstream = new XStream(new JsonHierarchicalStreamDriver());
				// #884 removed xstream.setMode(XStream.NO_REFERENCES);
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
			// CollectionConverter sait gérer de base ArrayList, HashSet et autres mais pas Collections.emptyList()
			final CollectionConverter collectionConverter = new CollectionConverter(
					xstream.getMapper()) {
				@SuppressWarnings("rawtypes")
				@Override
				public boolean canConvert(Class type) {
					// on pourrait accepter aussi les classes de Arrays.asList, List.of, etc
					if (type != null) {
						return Collections.emptyList().getClass().equals(type);
					}
					return super.canConvert(type);
				}

				@Override
				public Object unmarshal(final HierarchicalStreamReader reader,
						final UnmarshallingContext context) {
					// selon context.getRequiredType(), on pourrait utiliser Collections.emptyList() ou Arrays.asList ou List.of etc,
					// mais pour nous, Collections.emptyList() fera l'affaire
					return Collections.emptyList();
				}
			};
			xstream.registerConverter(collectionConverter);
			return xstream;
		}
	}

	// classe interne pour qu'elle ne soit pas chargée avec la classe TransportFormat
	// et qu'ainsi on ne dépende pas de GSON si on ne se sert pas du format gson
	// ni de XStream si on ne se sert pas du format json
	private static final class GsonIO {
		private static final Charset GSON_CHARSET = StandardCharsets.UTF_8;

		private GsonIO() {
			super();
		}

		static void writeToGson(Serializable serializable, BufferedOutputStream bufferedOutput)
				throws IOException {
			final JsonSerializer<StackTraceElement> stackTraceElementJsonSerializer = (src,
					typeOfSrc, context) -> new JsonPrimitive(src.toString());
			final Gson gson = new GsonBuilder()
					// .setPrettyPrinting() : prettyPrinting pas nécessaire avec un viewer de json
					.registerTypeAdapter(StackTraceElement.class, stackTraceElementJsonSerializer)
					.create();
			try (OutputStreamWriter writer = new OutputStreamWriter(bufferedOutput, GSON_CHARSET)) {
				gson.toJson(serializable, writer);
			}
		}
	}

	private static class MyObjectInputStream extends ObjectInputStream {
		private static final String PACKAGE_NAME = TransportFormat.class.getName().substring(0,
				TransportFormat.class.getName().length()
						- TransportFormat.class.getSimpleName().length() - 1);

		MyObjectInputStream(InputStream input) throws IOException {
			super(input);
		}

		// during deserialization, protect ourselves from malicious payload
		// http://www.ibm.com/developerworks/library/se-lookahead/index.html
		@Override
		protected Class<?> resolveClass(ObjectStreamClass desc)
				throws IOException, ClassNotFoundException {
			final String name = desc.getName();
			int i = 0;
			if (name.indexOf("[[") == 0) {
				// 2 dimensions array
				i++;
			}
			if (name.indexOf("[L", i) == i) {
				// 1 dimension array
				i += 2;
			}
			if (name.indexOf("java.lang.", i) == i || name.indexOf("java.util.", i) == i
					|| name.indexOf("java.io.", i) == i || name.indexOf(PACKAGE_NAME, i) == i
					|| name.length() <= 2) {
				// if name.length() == 2, primitive type or array (such as [B in javamelody-swing)
				return super.resolveClass(desc);
			} else if (name.indexOf("net.bull.javamelody", i) == i) {
				// resolve old classes names to new classes names, for backward compatibility
				return Class.forName(
						name.replace("net.bull.javamelody", "net.bull.javamelody.internal.model"));
			}
			throw new ClassNotFoundException(name);
		}
	}

	private final String code; // NOPMD
	private final String mimeType; // NOPMD

	TransportFormat(String mimeType) {
		this.mimeType = mimeType;
		this.code = this.toString().toLowerCase(Locale.ENGLISH);
	}

	public static TransportFormat valueOfIgnoreCase(String transportFormat) {
		return valueOf(transportFormat.toUpperCase(Locale.ENGLISH).trim());
	}

	public static boolean isATransportFormat(String format) {
		if (format == null) {
			return false;
		}
		final String upperCase = format.toUpperCase(Locale.ENGLISH).trim();
		for (final TransportFormat transportFormat : values()) {
			if (transportFormat.toString().equals(upperCase)) {
				return true;
			}
		}
		return false;
	}

	public String getCode() {
		return code;
	}

	public String getMimeType() {
		return mimeType;
	}

	public void checkDependencies() throws IOException {
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
		if (this == GSON) {
			try {
				Class.forName("com.google.gson.Gson");
			} catch (final ClassNotFoundException e) {
				throw new IOException(
						"Classes of the Gson library not found. Add the Gson dependency in your webapp for the GSON format.",
						e);
			}

		}
	}

	public void writeSerializableTo(Serializable serializable, OutputStream output)
			throws IOException {
		final Serializable nonNullSerializable;
		if (serializable == null) {
			nonNullSerializable = NULL_VALUE;
		} else {
			nonNullSerializable = serializable;
		}
		final BufferedOutputStream bufferedOutput = new BufferedOutputStream(output);
		switch (this) {
		case SERIALIZED:
			try (ObjectOutputStream out = new ObjectOutputStream(bufferedOutput)) {
				out.writeObject(nonNullSerializable);
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
		case GSON:
			GsonIO.writeToGson(nonNullSerializable, bufferedOutput);
			break;
		default:
			throw new IllegalStateException(toString());
		}
	}

	Serializable readSerializableFrom(InputStream input)
			throws IOException, ClassNotFoundException {
		final InputStream bufferedInput = new BufferedInputStream(input);
		final Object result;
		switch (this) {
		case SERIALIZED:
			try (ObjectInputStream in = createObjectInputStream(bufferedInput)) {
				result = in.readObject();
			}
			break;
		case XML:
			result = XmlIO.readFromXml(bufferedInput);
			break;
		case JSON:
			// pas possible avec JsonHierarchicalStreamDriver
			// (http://x-stream.github.io/json-tutorial.html)
			throw new UnsupportedOperationException();
		case GSON:
			throw new UnsupportedOperationException();
		default:
			throw new IllegalStateException(toString());
		}
		if (NULL_VALUE.equals(result)) {
			return null;
		}
		// c'est un Serializable que l'on a écrit
		return (Serializable) result;
	}

	static ObjectInputStream createObjectInputStream(InputStream input) throws IOException {
		return new MyObjectInputStream(input);
	}
}
