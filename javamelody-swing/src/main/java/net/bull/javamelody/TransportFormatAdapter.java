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
package net.bull.javamelody;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;

import net.bull.javamelody.internal.model.TransportFormat;

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
	public static void writeJson(Serializable serializable, OutputStream output)
			throws IOException {
		TransportFormat.JSON.writeSerializableTo(serializable, output);
	}
}
