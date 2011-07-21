package net.bull.javamelody;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;

/**
 * Adapter pour appeler TransportFormat qui est non public.
 * @author Emeric Vernat
 */
public final class TransportFormatAdapter {
	private TransportFormatAdapter() {
		super();
	}

	/**
	 * Export XML.
	 * @param serializable Serializable
	 * @param output OutputStream
	 * @throws IOException e
	 */
	public static void writeXml(Serializable serializable, OutputStream output) throws IOException {
		TransportFormat.XML.writeSerializableTo(serializable, output);
	}

	/**
	 * Export JSON.
	 * @param serializable Serializable
	 * @param output OutputStream
	 * @throws IOException e
	 */
	public static void writeJson(Serializable serializable, OutputStream output) throws IOException {
		TransportFormat.JSON.writeSerializableTo(serializable, output);
	}
}
