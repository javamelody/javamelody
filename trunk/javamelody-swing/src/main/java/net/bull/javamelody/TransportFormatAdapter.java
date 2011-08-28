/*
 * Copyright 2008-2010 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
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
